{-# LANGUAGE
  DeriveGeneric
#-}

module Pipend.Server (
    main
  , QueryType(..)
  , PC.ExecutableQuery(..)
  , Interface
  , InterfaceResult
  , startTask
  , killTask
  , TaskId
  , toString
) where
import Data.Either (either)
-- import Text.Read (readEither)
import qualified Control.Concurrent as C
import qualified Data.Map as M
import Control.Monad (join)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as C8L

import qualified Pipend.Connections as PC
import qualified Pipend.Connections.Curl as Curl
import qualified Pipend.Connections.PostgreSQL as PostgreSQL

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM

import qualified Data.Aeson as A
import GHC.Generics

type TaskId = String
data QueryType =
    PostgreSQL String PC.ExecutableQuery
  | Curl String
  deriving (Generic, Show)
instance A.FromJSON QueryType
instance A.ToJSON QueryType

getRunnerFromQueryType :: QueryType -> PC.RunIO PC.QueryRunner
getRunnerFromQueryType (PostgreSQL connectionString xQuery) =
  let con = PostgreSQL.PostgreSQLConnection connectionString
  in PC.executeQuery con xQuery
getRunnerFromQueryType (Curl command) =
  PC.executeQuery Curl.CurlConnection (PC.ExecutableQuery ("curl " ++ command) M.empty)


toString :: PC.QueryResult -> String
toString (PC.StringResult result) = result
toString (PC.JSONResult result) = C8L.unpack (JSON.encode result)


appV :: (a -> a) -> STM.TVar a -> IO ()
appV fn x = STM.atomically $ STM.readTVar x >>= STM.writeTVar x . fn

appT :: (a -> (a, IO b)) -> STM.TVar a -> IO b
appT fn x = join $ STM.atomically $ do
  x' <- STM.readTVar x
  let (x'', b) = fn x'
  STM.writeTVar x x''
  return b

-- data UserCommand = Add TaskId (String, String) | Kill String
type QueryOut = Either PC.SomeError PC.QueryResult
type TasksDic = M.Map TaskId (C.ThreadId, PC.QueryCanceller, MVar.MVar QueryOut)

start :: STM.TVar TasksDic -> TaskId -> PC.QueryRunner -> PC.RunIO QueryOut
start dic name runner = do
  m <- PC.liftRunIO  MVar.newEmptyMVar
  t <- PC.liftRunIO $ C.forkIO $ do
    result <- PC.runIO (PC.run runner)
    M.delete name `appV` dic
    MVar.putMVar m result
  PC.liftRunIO $ M.insert name (t, PC.cancel runner, m) `appV` dic
  PC.liftRunIO $ MVar.takeMVar m

kill :: STM.TVar TasksDic -> TaskId -> PC.RunIO QueryOut
kill dic name = PC.liftRunIO $ appT processKill dic where
  processKill :: TasksDic -> (TasksDic, IO QueryOut)
  processKill dic = maybe
    (dic, return $ Left ("Task " ++ name ++ " not found!"))
    (
      \(t, c, m) -> (M.delete name dic, do
          let sendMessage verb = do
                let message = "Task " ++ name ++ " " ++ verb
                MVar.putMVar m $ Left message
                return $ Left message
          cancelResult <- PC.runIO c
          case cancelResult of
            Left e -> do
                C.killThread t
                sendMessage $ "Cancelling Error: " ++ e
            Right _ -> sendMessage "Cancelled"
        )
    )
    (M.lookup name dic)


go i serverLoop = do
  -- print $ "go loop " ++ show i
  MVar.takeMVar serverLoop
  go (i+1) serverLoop

data Interface = Interface {
    startTask :: TaskId -> QueryType -> IO InterfaceResult
  , killTask  :: TaskId -> IO InterfaceResult
}

type InterfaceResult = Either PC.SomeError PC.QueryResult

startTask' :: STM.TVar TasksDic -> MVar.MVar () -> TaskId -> QueryType -> IO InterfaceResult
startTask' dic serverLoop taskId queryType = do
  MVar.putMVar serverLoop ()
  fmap join $ PC.runIO $ start dic taskId =<< getRunnerFromQueryType queryType

killTask' :: STM.TVar TasksDic -> MVar.MVar () -> TaskId -> IO InterfaceResult
killTask' dic serverLoop taskId = do
  MVar.putMVar serverLoop ()
  fmap join $ PC.runIO $ kill dic taskId



main :: IO Interface
main = do
  serverLoop <- MVar.newEmptyMVar
  dic <- STM.atomically $ STM.newTVar M.empty
  -- addTask :: IO () -> IO ()
  -- let addTask' = addTask dic serverLoop

  let interface = Interface {
      startTask = startTask' dic serverLoop
    , killTask = killTask' dic serverLoop
  }

  C.forkIO $ go 0 serverLoop
  return interface


main' = do
  interface <- main
  let start = startTask interface
  let kill  = killTask interface
  C.forkIO $ do
    -- taskResult <- adder $ Add "a" ("sql", "(\"postgres://marketing:XN&Ho)pxp=7@jewelbox.cit562lelvpp.us-east-1.redshift.amazonaws.com:5439/rockman\", \"select country_code, count(*) as events from events where timestamp > '2016-06-01' group by country_code limit 10\")")
    taskResult <- start "taskId1" $
      PostgreSQL
        "postgres://127.0.0.1"
        $ PC.ExecutableQuery
          "select 10 * 12"
          M.empty
    putStrLn $ "Task Result = " ++ either show toString taskResult
  C.forkIO $ do
    C.threadDelay $ 2 * 1000000
    putStrLn " ****** "
    killResult <- kill "taskId1"
    C.threadDelay $ 1 * 1000000
    putStrLn $ "killResult = " ++ either show toString killResult
  -- adder $ print "hello2"
  return ()
  -- adder

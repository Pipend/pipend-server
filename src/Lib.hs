module Lib (
    main, getRunner
  , appV, appT
  , toString
) where

import Data.Either (either)
import qualified Control.Concurrent as C
import qualified Data.Map as M
import qualified Data.List.Split as S
import qualified Control.Concurrent.STM as STM
import Control.Monad (join)
import qualified System.IO
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as C8L

import qualified Pipend.Connections as PConnections
import qualified Pipend.Connections.Curl as Curl
import qualified Pipend.Connections.PostgreSQL as PostgreSQL

data UserCommand = Add String (String, String) | Kill String

parseInput :: String -> UserCommand
parseInput userInput =
  let
      args@(cmd:_) = S.splitOn " " userInput
  in  go cmd (drop 1 args) where
      go "add" args@(name:queryType:_) = Add name (queryType, unwords $ drop 2 args)
      go "kill" (name:_) = Kill name
      go _ _ = error "invalid input"

main' :: STM.TVar (M.Map String (C.ThreadId, PConnections.QueryCanceller)) -> IO ()
main' dic = do
  userInput <- parseInput <$> getLine
  PConnections.runIO $ process userInput
  main' dic

  where
    process :: UserCommand -> PConnections.RunIO ()
    process (Add name (queryType, queryArgs)) = do
      PConnections.liftRunIO $ putStrLn $ "queryType = " ++ queryType
      PConnections.liftRunIO $ putStrLn $ "queryArgs = " ++ queryArgs
      runner <- getRunner queryType queryArgs
      t <- PConnections.liftRunIO $ C.forkIO $ do
        result <- PConnections.runIO (PConnections.run runner)
        M.delete name `appV` dic
        putStrLn $ either ("ERROR: "++) toString result
      PConnections.liftRunIO $ putStrLn $ "TaskId " ++ show t
      PConnections.liftRunIO $ M.insert name (t, PConnections.cancel runner) `appV` dic
    process (Kill name) = PConnections.liftRunIO $ appT processKill dic where
      processKill :: M.Map String (C.ThreadId, PConnections.QueryCanceller) -> (M.Map String (C.ThreadId, PConnections.QueryCanceller), IO ())
      processKill dic = maybe
        (dic, putStrLn ("Task " ++ name ++ " not found!"))
        (
          \(t, c) -> (M.delete name dic, do
              putStrLn "Kiling ..."
              cancelResult <- PConnections.runIO c
              case cancelResult of
                Left e -> do
                    putStrLn $ "Cancelling Error: " ++ e
                    C.killThread t
                Right _ -> putStrLn "Cancelled"
              -- C.killThread t
              putStrLn "Killed"
            )
        )
        (M.lookup name dic)

getRunner :: String -> String -> PConnections.RunIO PConnections.QueryRunner -- IO (Maybe String)
getRunner "curl" args =
  PConnections.executeQuery Curl.CurlConnection (PConnections.ExecutableQuery ("curl " ++ args) M.empty)
-- getRunner "curl" args = fmap toString <$> PConnections.executeQuery Curl.CurlConnection (PConnections.ExecutableQuery ("curl " ++ args) M.empty)
getRunner "sql" sargs = do
  let (connectionString, args) = read sargs
  let con = PostgreSQL.PostgreSQLConnection connectionString
  PConnections.executeQuery con (PConnections.ExecutableQuery args M.empty)
  -- fmap toString <$>
getRunner _ _ = PConnections.throwRunIO "Invalid query type"

toString :: PConnections.QueryResult -> String
toString (PConnections.StringResult result) = result
toString (PConnections.JSONResult result) = C8L.unpack (JSON.encode result)


appV :: (a -> a) -> STM.TVar a -> IO ()
appV fn x = STM.atomically $ STM.readTVar x >>= STM.writeTVar x . fn

appT :: (a -> (a, IO b)) -> STM.TVar a -> IO b
appT fn x = join $ STM.atomically $ do
  x' <- STM.readTVar x
  let (x'', b) = fn x'
  STM.writeTVar x x''
  return b

main :: IO ()
main = do
  System.IO.hSetBuffering System.IO.stdin System.IO.LineBuffering
  System.IO.hSetBuffering System.IO.stdout System.IO.LineBuffering
  dic <- STM.atomically $ STM.newTVar M.empty
  main' dic

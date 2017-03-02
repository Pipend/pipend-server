module Lib (
  main
) where

import Data.Maybe (fromMaybe)
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

parseInput userInput =
  let
      args@(cmd:_) = S.splitOn " " userInput
  in  go cmd (drop 1 args) where
      go "add" args@(name:queryType:_) = Add name (queryType, unwords $ drop 2 args)
      go "kill" (name:_) = Kill name
      go _ _ = error "invalid input"

main' :: STM.TVar (M.Map String C.ThreadId) -> IO ()
main' dic = do
  userInput <- parseInput <$> getLine
  process userInput
  main' dic

  where
    process (Add name (queryType, queryArgs)) = do
      putStrLn $ "queryType = " ++ queryType
      putStrLn $ "queryArgs = " ++ queryArgs
      t <- C.forkIO $ do
        result <- runQuery queryType queryArgs
        M.delete name `appV` dic
        putStrLn $ fromMaybe "ERROR" result
      putStrLn $ "TaskId " ++ show t
      M.insert name t `appV` dic
    process (Kill name) = appT processKill dic where
      processKill :: M.Map String C.ThreadId -> (M.Map String C.ThreadId, IO ())
      processKill dic = maybe
        (dic, putStrLn ("Task " ++ name ++ " not found!"))
        (
          \t -> (M.delete name dic, C.killThread t >> putStrLn "Killed")
        )
        (M.lookup name dic)

runQuery :: String -> String -> IO (Maybe String)
runQuery "curl" args = fmap toString <$> PConnections.executeQuery Curl.CurlConnection (PConnections.ExecutableQuery ("curl " ++ args) M.empty)
runQuery "sql" sargs = do
  let (connectionString, args) = read sargs
  let con = PostgreSQL.PostgreSQLConnection connectionString
  fmap toString <$> PConnections.executeQuery con (PConnections.ExecutableQuery args M.empty)
runQuery _ _ = error "invalid query type"

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


main = do
  System.IO.hSetBuffering System.IO.stdin System.IO.LineBuffering
  System.IO.hSetBuffering System.IO.stdout System.IO.LineBuffering
  dic <- STM.atomically $ STM.newTVar M.empty
  main' dic

{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , OverloadedStrings
#-}

module TestWebServer (
  main
) where

import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Reader (MonadReader, lift, liftIO, ask)
import qualified Data.Map as M
import Data.Text.Lazy (Text, pack, unpack)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Web.Scotty.Trans
import Network.Wai.Middleware.RequestLogger
import qualified Data.Aeson as A
import qualified Pipend.Server as Server
import qualified Pipend.Config.Connections as Connections

type AppState = Server.Interface

newtype WebM a = WebM { runWebM :: ReaderT  AppState IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState)

startTask :: Server.TaskId -> Server.QueryType -> ActionT Text WebM Server.InterfaceResult
startTask taskId command = do
  interface <- lift ask
  let start = Server.startTask interface
  liftIO $ start taskId command

killTask :: Server.TaskId -> ActionT Text WebM Server.InterfaceResult
killTask taskId = fmap Server.killTask (lift ask) >>= liftIO . ($ taskId)

app :: ScottyT Text WebM ()
app = do
  middleware logStdoutDev

  get "/api/kill/:id" $ do
    taskId <- param "id"
    taskResult <- killTask taskId
    either
      (text . pack)
      (text . pack . Server.toString)
      taskResult

  post "/api/query/arbitrary/:id" $ do
    input <- jsonData :: ActionT Text WebM Server.QueryType
    taskId <- param "id"
    taskResult <- startTask taskId input
    either
      (text . pack)
      (text . pack . Server.toString)
      taskResult

  post "/api/query/connection/:connectionId/taskid/:id" $ do
    taskId <- param "id" :: ActionT Text WebM Server.TaskId
    connectionId <- param "connectionId"
    connection <- liftIO $  (M.lookup connectionId =<<) <$> Connections.getConnections
    input <- jsonData :: ActionT Text WebM Server.ExecutableQuery
    maybe
      (text "Connection not found")
      (\ connection -> do
        let queryType = Server.QueryType connection input
        taskResult <- startTask taskId queryType
        either
          (text . pack)
          (text . pack . Server.toString)
          taskResult
      )
      connection

  post "/test/:a" $ do
    b <- decodeUtf8 <$> body
    a <- param "a"
    liftIO $ putStrLn $ unpack b
    text $ TL.concat [b, "\n", a]

  -- list all connections
  get "/" $ liftIO Connections.getConnections >>= text . maybe "error" (decodeUtf8 . A.encode)


main :: IO ()
main = do
  interface <- Server.main
  let runActionToIO m = runReaderT (runWebM m) interface
  scottyT 3000 runActionToIO app

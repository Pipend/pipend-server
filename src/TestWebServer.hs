{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , OverloadedStrings
  , DeriveGeneric
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
import qualified Pipend.Server as Server
import qualified Data.Aeson as A
import GHC.Generics

data PostgreSQLInput = PostgreSQLInput {
    connectionString :: String
  , query :: String
} deriving (Generic, Show)
instance A.FromJSON PostgreSQLInput
instance A.ToJSON PostgreSQLInput

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
  get "/add/postgresql/:id" $ do
    taskId <- param "id"
    taskResult <- startTask taskId $ Server.PostgreSQL
      "postgres://127.0.0.1"
      $ Server.ExecutableQuery
        "select 10 * 12"
        M.empty
    either
      (text . pack)
      (text . pack . Server.toString)
      taskResult

  get "/kill/:id" $ do
    taskId <- param "id"
    taskResult <- killTask taskId
    either
      (text . pack)
      (text . pack . Server.toString)
      taskResult

  -- fmap A.encode (A.decode (C8L.pack params) :: Maybe A.Value)
  post "/test/sql/:id" $ do
    input <- jsonData :: ActionT Text WebM Server.QueryType
    taskId <- param "id"
    taskResult <- startTask taskId input
    either
      (text . pack)
      (text . pack . Server.toString)
      taskResult

    -- text $ decodeUtf8 $ A.encode input

    -- queryParams <- (fromMaybe M.empty . A.decode <$> param "params") :: ActionT Text WebM (M.Map String A.Value)
    -- text $ decodeUtf8 $ A.encode input
    -- text $ decodeUtf8 $ A.encode $ Server.PostgreSQL
    --   "localhost"
    --   $ Server.ExecutableQuery
    --     "select 5 * 7"
    --     queryParams

  post "/test/:a" $ do
    b <- decodeUtf8 <$> body
    a <- param "a"
    liftIO $ putStrLn $ unpack b
    text $ TL.concat [b, "\n", a]



main :: IO ()
main = do
  interface <- Server.main
  let runActionToIO m = runReaderT (runWebM m) interface
  scottyT 3000 runActionToIO app

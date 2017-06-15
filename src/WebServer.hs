{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , OverloadedStrings
#-}

module WebServer (
  main
) where

import Data.Either (either)
import qualified Control.Concurrent as C
import qualified Control.Concurrent.MVar as MV
import qualified Control.Concurrent.STM as STM
import qualified Control.Monad.Trans.Reader as R
import Control.Monad.Trans (MonadIO)
import Control.Applicative (Applicative(..))
import Control.Monad.Reader (MonadReader, MonadTrans, lift, liftIO, ask)
import qualified Data.Map as M
import qualified Lib

import qualified Pipend.Connections as PConnections

import Web.Scotty.Trans
import Data.String
import Data.Text.Lazy (Text, pack)


type TasksDic = M.Map String (C.ThreadId, PConnections.QueryCanceller, MV.MVar (Either PConnections.SomeError PConnections.QueryResult))
newtype AppState = AppState { appState :: TasksDic }

newtype WebM a = WebM { runWebM :: R.ReaderT (STM.TVar AppState) IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (STM.TVar AppState))

webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

gets :: (AppState -> b) -> WebM b
gets f = fmap f (ask >>= liftIO . STM.readTVarIO)

modify :: (AppState -> AppState) -> WebM ()
modify f = ask >>= liftIO . STM.atomically . flip STM.modifyTVar' f

app :: ScottyT Text WebM ()
app = do
    let name = "A"
    -- middleware logStdoutDev
    get "/" $ text $ fromString "got the request"
    get "/add" $ do
      let runner = Lib.getRunner "sql" "(\"\", \"select 5 + 6\")"
      runner' <- liftIO $ PConnections.runIO runner
      either
        error --TODO: fix me
        (execRunner name)
        runner'
    get "/cancel" $ do
      dic <- lift $ gets id
      maybe
        (text $ pack (name ++ " Not Found"))
        (
          \ (tid, canceller, m) -> do
            lift $ modify $ AppState . M.delete name . appState
            cancelResult <- liftIO $ PConnections.runIO canceller
            case cancelResult of
              Left e -> do
                  liftIO $ C.killThread tid
                  let errorText = "Cancelling Error: " ++ e
                  liftIO $ MV.putMVar m (Left errorText)
                  text $ pack errorText
              Right _ -> do
                let errorText = name ++ " Cancelled"
                liftIO $ MV.putMVar m (Left errorText)
                text $ pack errorText
        )
        (M.lookup name $ appState dic)


execRunner :: String -> PConnections.QueryRunner -> ActionT Text WebM ()
execRunner name runner = do
  m <- liftIO MV.newEmptyMVar
  t <- liftIO $ C.forkIO $ do
      result <- PConnections.runIO (PConnections.run runner)
      MV.putMVar m result
  webM $ modify (AppState . M.insert name (t, PConnections.cancel runner, m) . appState)
  r <- liftIO $ MV.takeMVar m
  webM $ modify (AppState . M.delete name . appState)
  either
    (text . pack)
    (text . pack . Lib.toString)
    r

main :: IO ()
main = do
  dic <- STM.newTVarIO AppState { appState = M.empty }
  let runActionToIO m = R.runReaderT (runWebM m) dic
  scottyT 3000 runActionToIO app

{-# LANGUAGE
    MultiParamTypeClasses
  , OverloadedStrings
  , DeriveGeneric
#-}

module Pipend.Connections.PostgreSQL (
  PostgreSQLConnection(..)
) where

import Pipend.Connections

import qualified Database.PostgreSQL.LibPQ as PQ
import Foreign.C.Types (CUInt)
import Control.Applicative (liftA2)
import qualified Data.ByteString.Lazy.Char8 as C8L
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text.Encoding as TE
import Control.Arrow ((+++), (|||))
import Control.Monad (join)
import Data.Maybe (fromMaybe)
import GHC.Exts (fromList)
import GHC.Generics (Generic)
import qualified Data.Aeson as A

-- sql template to sql
-- (EX.try (Process.readProcessWithExitCode "./node/Pipend/Connection-Utils/sql-template.js" [""] "") :: IO (Either EX.SomeException (ExitCode, String, String ))) >>= print

newtype PostgreSQLConnection = PostgreSQLConnection {
  connectionString :: String
} deriving (Show, Generic)
instance A.FromJSON PostgreSQLConnection
instance A.ToJSON PostgreSQLConnection

instance IsConnection PostgreSQLConnection where
  executeQuery conn query = do
    conn <- liftRunIO $ PQ.connectdb (C8.pack $ connectionString conn)
    return QueryRunner {
      run = liftRunIO (runPGQuery conn query) >>= throwRunIO ||| (liftRunIO . resToJSON)
    , cancel = liftRunIO (cancelPGQuery conn) >>= throwRunIO ||| return
    }

cancelPGQuery :: PQ.Connection -> IO (Either String ())
cancelPGQuery conn = PQ.getCancel conn >>= maybe
  (return $ Left "Unable to retrieve PostgreSQL cancel token.")
  (joinE . fmap (return . C8.unpack +++ const (PQ.finish conn)) . PQ.cancel)

runPGQuery :: PQ.Connection -> ExecutableQuery -> IO (Either String PQ.Result)
runPGQuery conn query = do
  -- print =<< PQ.backendPID conn
  res <- PQ.exec conn $ C8.pack $ executableQueryText query
  PQ.finish conn
  return $ maybe (Left "Error in querying the database") Right res

resToJSON :: PQ.Result -> IO QueryResult
resToJSON res = do
  rows <- enumFromTo (PQ.toRow 0) . pred <$> PQ.ntuples res
  cols <- enumFromTo (PQ.toColumn 0) . pred <$> PQ.nfields res
  columns <- mapM (\c -> do
    t <- PQ.ftype res c
    n <- PQ.fname res c
    return (c, t, fromMaybe "error" n)
    ) cols

  json <- A.Array . fromList <$> mapM (rowToJSON res columns) rows
  return $ JSONResult json

  where

  rowToJSON :: PQ.Result -> [(PQ.Column, PQ.Oid, C8.ByteString)] -> PQ.Row -> IO A.Value
  rowToJSON res cols row = A.Object . fromList <$> mapM
    (\ (col, PQ.Oid i, name) ->
        (,) (showName name)
        <$> (maybe A.Null (showCell i) <$> PQ.getvalue res row col)
    )
    cols

  showCell :: CUInt -> C8.ByteString -> A.Value
  showCell 705 = A.String . TE.decodeUtf8
  showCell 16 = \ v -> A.Bool $ "t" == C8.unpack v
  -- first try to parse to cell to an arbitrary JSON value, then String
  showCell _ = liftA2 fromMaybe stringVal tryJson where
    tryJson = A.decode . C8L.fromStrict
    stringVal = A.String . TE.decodeUtf8

  showName = TE.decodeUtf8


-- Arrow utility functions

uncozipL :: Functor m => Either (m a) (m b) -> m (Either a b)
uncozipL = fmap Left ||| fmap Right

joinE :: Monad m => m (Either (m a)  (m b)) -> m (Either a b)
joinE = join . fmap uncozipL

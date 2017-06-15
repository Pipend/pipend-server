{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}

module Pipend.Connections.PostgreSQL (
  PostgreSQLConnection(..)
) where

import Pipend.Connections

import qualified Database.PostgreSQL.LibPQ as PQ
import Foreign.C.Types (CUInt)
import Control.Applicative (liftA2)
-- import Control.Monad ((>>))

import qualified Data.ByteString.Lazy.Char8 as C8L
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text.Encoding as TE

import Data.Maybe (fromMaybe)
import GHC.Exts (fromList)

import qualified Data.Aeson as JSON

data PostgreSQLConnection = PostgreSQLConnection {
  postgreSqlConnectionString :: String
}

instance IsConnection PostgreSQLConnection where
  executeQuery conn query = do
    conn <- liftRunIO $ PQ.connectdb (C8.pack $ postgreSqlConnectionString conn)
    return QueryRunner {
      run = do
        liftRunIO $ print =<< PQ.backendPID conn
        res <- liftRunIO $ PQ.exec conn $ C8.pack $ executableQueryText query
        liftRunIO $ PQ.finish conn
        maybe
          (throwRunIO "Error in querying the database")
          (liftRunIO . resToJSON)
          res

    , cancel = do
        cancel <- liftRunIO $ PQ.getCancel conn
        maybe
          (throwRunIO "Error cancelling")
          (\c -> do
            c' <- liftRunIO $ PQ.cancel c
            either
              (throwRunIO . C8.unpack)
              (\ r -> liftRunIO (PQ.finish conn) >> return r)
              c'
          )
          cancel

    }


resToJSON :: PQ.Result -> IO QueryResult
resToJSON res = do
  rows <- enumFromTo (PQ.toRow 0) . pred <$> PQ.ntuples res
  cols <- enumFromTo (PQ.toColumn 0) . pred <$> PQ.nfields res
  columns <- mapM (\c -> do
    t <- PQ.ftype res c
    n <- PQ.fname res c
    return (c, t, fromMaybe "error" n)
    ) cols

  json <- JSON.Array . fromList <$> mapM (rowToJSON res columns) rows
  return $ JSONResult json

  where

  rowToJSON :: PQ.Result -> [(PQ.Column, PQ.Oid, C8.ByteString)] -> PQ.Row -> IO JSON.Value
  rowToJSON res cols row = JSON.Object . fromList <$> mapM
    (\ (col, PQ.Oid i, name) ->
        (,) (showName name)
        <$> (maybe JSON.Null (showCell i) <$> PQ.getvalue res row col)
    )
    cols

  showCell :: CUInt -> C8.ByteString -> JSON.Value
  showCell 705 = JSON.String . TE.decodeUtf8
  showCell 16 = \ v -> JSON.Bool $ "t" == C8.unpack v
  -- first try to parse to cell to an arbitrary JSON value, then String
  showCell _ = liftA2 fromMaybe stringVal tryJson where
    tryJson = JSON.decode . C8L.fromStrict
    stringVal = JSON.String . TE.decodeUtf8

  showName = TE.decodeUtf8

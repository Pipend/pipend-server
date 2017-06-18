{-# LANGUAGE
  DeriveGeneric
#-}

module Pipend.Query (
  Connections (..)
, Connection (..)
, Query (..)
) where

import qualified Pipend.Connections as Connections
import qualified Pipend.Connections.Curl as Curl
import qualified Pipend.Connections.PostgreSQL as PQL
import qualified Pipend.Policy as Policy
import qualified Pipend.Connections as PC
import GHC.Generics (Generic)
import qualified Data.Aeson as A

data Connections =
    PostgreSQL PQL.PostgreSQLConnection
  | Curl Curl.CurlConnection
  deriving (Generic, Show)
instance A.FromJSON Connections
instance A.ToJSON Connections

instance PC.IsConnection Connections where
  executeQuery (PostgreSQL c) = PC.executeQuery c
  executeQuery (Curl c) = PC.executeQuery c 

data Connection = Connection {
    connection :: Connections
  , connectionAccessPolicy :: Policy.AccessPolicy
  , connectionExecutionPolicy :: Policy.ExecutionPolicy
}

data Query = Query {
    executableQuery :: Connections.ExecutableQuery
  , queryTransformation :: String
  , queryPresentation :: String
  , queryExecute :: Connection
}

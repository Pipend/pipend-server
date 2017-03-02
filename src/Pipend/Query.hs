module Pipend.Query (
  Connections (..)
, Connection (..)
, Query (..)
) where

import qualified Pipend.Connections as Connections
import qualified Pipend.Connections.Curl as Curl
import qualified Pipend.Connections.PostgreSQL as PostgreSQL
import qualified Pipend.Policy as Policy

data Connections =
    PostgreSQL PostgreSQL.PostgreSQLConnection
  | Curl Curl.CurlConnection

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

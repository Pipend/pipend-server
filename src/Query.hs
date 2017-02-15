module Query (
    Connection(..), Query(..)
) where

import qualified Data.Map as M

import qualified Pipend.Policy as Policy

data ExecutableQuery = ExecutableQuery {
    executableQueryText :: String
  , executableQueryParams :: M.Map String String
}

newtype ConnectionType = ConnectionType {
  executeQuery :: ExecutableQuery -> IO String
}

data Connection = Connection {
    connectionType :: ConnectionType
  , connectionAccessPolicy :: Policy.AccessPolicy
  , connectionExecutionPolicy :: Policy.ExecutionPolicy
}

data Query = Query {
    executableQuery :: ExecutableQuery
  , queryTransformation :: String
  , queryPresentation :: String
  , queryExecute :: Connection
}

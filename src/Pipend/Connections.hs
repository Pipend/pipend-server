{-# LANGUAGE MultiParamTypeClasses #-}

module Pipend.Connections (
    ExecutableQuery(..)
  , IsConnection(..)
  , QueryResult(..)
) where

import qualified Data.Map as M
import qualified Data.Aeson as JSON

data ExecutableQuery = ExecutableQuery {
    executableQueryText :: String
  , executableQueryParams :: M.Map String String
}

data QueryResult = JSONResult JSON.Value | StringResult String

class IsConnection conn where
  executeQuery :: conn -> ExecutableQuery -> IO (Maybe QueryResult)

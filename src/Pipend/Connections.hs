{-# LANGUAGE MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  , DeriveGeneric
#-}

module Pipend.Connections (
    ExecutableQuery(..)
  , IsConnection(..)
  , QueryResult(..)
  , QueryRunner(..)
  , SomeError
  , QueryCanceller
  , RunIO
  , runIO
  , liftRunIO
  , throwRunIO
) where

import qualified Data.Map as M
import qualified Data.Aeson as JSON
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Except as ME
import Control.Monad.Trans (liftIO)
import qualified Data.Aeson as A
import GHC.Generics

data ExecutableQuery = ExecutableQuery {
    executableQueryText :: String
  , executableQueryParams :: M.Map String A.Value
} deriving (Generic, Show)
instance A.FromJSON ExecutableQuery
instance A.ToJSON ExecutableQuery

data QueryResult = JSONResult JSON.Value | StringResult String deriving Show

type SomeError = String
type QueryCanceller = RunIO ()

data QueryRunner = QueryRunner {
    run :: RunIO QueryResult
  , cancel :: QueryCanceller
}


newtype RunIO a = RunIO {
  unIO :: E.ExceptT SomeError IO a
} deriving (Functor, Applicative, Monad, ME.MonadError SomeError, ME.MonadIO)


runIO :: RunIO a -> IO (Either SomeError a)
runIO = ME.runExceptT . unIO

liftRunIO :: IO a -> RunIO a
liftRunIO = liftIO

throwRunIO :: SomeError -> RunIO a
throwRunIO = RunIO . E.throwE

class IsConnection conn where
  executeQuery :: conn -> ExecutableQuery -> RunIO QueryRunner

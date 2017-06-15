{-# LANGUAGE MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
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

data ExecutableQuery = ExecutableQuery {
    executableQueryText :: String
  , executableQueryParams :: M.Map String String
}

data QueryResult = JSONResult JSON.Value | StringResult String

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

throwRunIO = RunIO . E.throwE
--
-- duck :: Int -> QR QueryRunner
-- duck 1 =  return QueryRunner {}
-- duck _ =  throwQR "Some Error"
--
-- main :: QR QueryRunner -> IO (Either SomeError QueryRunner)
-- main q = E.runExceptT (runQR q)

class IsConnection conn where
  executeQuery :: conn -> ExecutableQuery -> RunIO QueryRunner

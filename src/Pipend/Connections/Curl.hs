{-# LANGUAGE
    MultiParamTypeClasses
  , OverloadedStrings
  , FlexibleContexts
  , DeriveGeneric
#-}

module Pipend.Connections.Curl (
  CurlConnection (..)
) where

import Pipend.Connections

import qualified System.Process as Process
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Parsec.Char (space, noneOf)
import Control.Applicative ((*>))
import GHC.Generics (Generic)
import Control.Exception (try, SomeException)
import System.Exit (ExitCode(..))
import Control.Arrow ((|||))
import qualified Data.Aeson as A

data CurlConnection = CurlConnection deriving (Show, Generic)
instance A.FromJSON CurlConnection
instance A.ToJSON CurlConnection

instance IsConnection CurlConnection where
  executeQuery _ query =
    let args = regularParse parseArgs (executableQueryText query)
    in  case args of
      Right args -> return QueryRunner {
          run = StringResult <$> runCommand "curl" (drop 1 args) ""
        , cancel = throwRunIO "Cannot cancel curl requests"
      }
      Left err -> throwRunIO (show err)

runCommand :: FilePath -> [String] -> String -> RunIO String
runCommand path args stdin =
  (throwRunIO . show ||| handleExitCode) =<<
    liftRunIO (try $ Process.readProcessWithExitCode path args stdin :: IO (Either SomeException (ExitCode, String, String )))
  where
    handleExitCode (ExitSuccess, out, _) = return out
    handleExitCode (ExitFailure _, _, err) = throwRunIO err



regularParse :: Parser a -> String -> Either P.ParseError a
regularParse p = P.parse p ""

parseArgs :: Parser [String]
parseArgs = P.choice [
    P.try $ parseString >>= \ s -> P.many1 space >> ((s:) <$> parseArgs)
  , (:[]) <$> parseString
  , P.anyToken *> return []
  ]

parseString :: Parser String
parseString = P.choice [
    betweenQuotation '"'
  , betweenQuotation '\''
  , word
  ]

word :: Parser String
word = P.many1 $ P.satisfy (not . (`elem` [' ', '\t']))

nonQuotedString :: Char -> Parser String
nonQuotedString quote = P.many1 (noneOf [quote])

betweenQuotation :: Char -> Parser String
betweenQuotation quote = P.between
  (P.char quote)
  (P.char quote)
  (nonQuotedString quote)

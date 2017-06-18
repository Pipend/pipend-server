module Pipend.Config.Connections (
  getConnections
) where

import qualified Pipend.Connections.PostgreSQL as PostgreSQL
import qualified Pipend.Connections.Curl as Curl
import qualified Data.Map as M
import qualified Pipend.Query as PQ
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL

type ConnectionsMap = M.Map String PQ.Connections
connections :: ConnectionsMap
connections = M.fromList [
    ("localhost", PQ.PostgreSQL $ PostgreSQL.PostgreSQLConnection "postgres://127.0.0.1")
  , ("curl", PQ.Curl Curl.CurlConnection )
  ]

getConnections :: IO (Maybe ConnectionsMap)
getConnections = A.decode <$> BL.readFile "./configs/connections.json"

-- main = BL.putStrLn $ A.encode connections
--  putStrLn $ TL.unpack $ decodeUtf8

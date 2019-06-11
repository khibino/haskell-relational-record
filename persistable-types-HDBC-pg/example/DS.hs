{-# LANGUAGE TemplateHaskell #-}

module DS (definePgTable) where

import Language.Haskell.TH (Q, Dec, Name)
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Database.Relational.HDBC.TH (defineTableFromDB)
import Database.Schema.HDBC.Driver (Driver (..))
import Database.Schema.HDBC.PostgreSQL (driverPostgreSQL)
import Data.PostgreSQL.NetworkAddress

addNetAddress :: Driver conn -> Driver conn
addNetAddress d =
  d { typeMap = [ ("inet", [t| Inet |]), ("cidr", [t| Cidr |] ) ] ++ typeMap d }

definePgTable :: String -> String -> [Name] -> Q [Dec]
definePgTable =
  defineTableFromDB (connectPostgreSQL "dbname=testdb") (addNetAddress driverPostgreSQL)

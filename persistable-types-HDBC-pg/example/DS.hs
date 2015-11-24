{-# LANGUAGE TemplateHaskell #-}

module DS (definePgTable) where

import Language.Haskell.TH (Q, Dec)
import Language.Haskell.TH.Name.CamelCase (ConName)
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Database.HDBC.Query.TH (defineTableFromDB)
import Database.HDBC.Schema.Driver (Driver (..))
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import Data.PostgreSQL.NetworkAddress

addNetAddress :: Driver conn -> Driver conn
addNetAddress d =
  d { typeMap = [ ("inet", [t| Inet |]), ("cidr", [t| Cidr |] ) ] ++ typeMap d }

definePgTable :: String -> String -> [ConName] -> Q [Dec]
definePgTable =
  defineTableFromDB (connectPostgreSQL "dbname=testdb") (addNetAddress driverPostgreSQL)

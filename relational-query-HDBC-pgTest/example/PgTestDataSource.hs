
module PgTestDataSource (
  connect, defineTable
  ) where

import Language.Haskell.TH (Q, Dec, TypeQ, Name)
import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import Database.HDBC.Schema.Driver (typeMap)
import Database.HDBC.Query.TH (defineTableFromDB)

connect :: IO Connection
connect = connectPostgreSQL "dbname=testdb"

defineTable :: [(String, TypeQ)] -> String -> String -> [Name] -> Q [Dec]
defineTable tmap =
  defineTableFromDB
    connect
    (driverPostgreSQL { typeMap = tmap })

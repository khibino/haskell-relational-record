
module PgTestDataSource (
  connect, defineTable
  ) where

import Language.Haskell.TH (Q, Dec, TypeQ)
import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import Database.HDBC.Schema.Driver (typeMap)
import Language.Haskell.TH.Name.CamelCase (ConName)
import Database.HDBC.Query.TH (defineTableFromDB)

connect :: IO Connection
connect = connectPostgreSQL "dbname=testdb"

defineTable :: [(String, TypeQ)] -> String -> String -> [ConName] -> Q [Dec]
defineTable tmap scm tbl derives = do
  defineTableFromDB
    connect
    (driverPostgreSQL { typeMap = tmap })
    scm tbl derives

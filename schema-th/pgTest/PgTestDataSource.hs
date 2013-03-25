
module PgTestDataSource (
  defineTable
  ) where

import Language.Haskell.TH (Q, Dec, runIO)
import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import Database.HDBC.TH (ConName, pprQ, defineTableFromDB)

connect :: IO Connection
connect = connectPostgreSQL "dbname=testdb"

defineTable :: String -> String -> [ConName] -> Q [Dec]
defineTable scm tbl derives = do
  conn <- runIO connect
  defineTableFromDB driverPostgreSQL conn scm tbl derives

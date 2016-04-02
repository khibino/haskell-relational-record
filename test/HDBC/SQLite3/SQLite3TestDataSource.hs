module SQLite3TestDataSource (
    connect,
    defineTable
    ) where

import Database.HDBC.Query.TH (defineTableFromDB)
import Database.HDBC.Schema.Driver (typeMap)
import Database.HDBC.Schema.SQLite3 (driverSQLite3)
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import Language.Haskell.TH (Q, Dec, TypeQ)
import Language.Haskell.TH.Name.CamelCase (ConName)

{-# ANN module "HLint: ignore Eta reduce" #-}

connect :: IO Connection
connect = connectSqlite3 "test/testdb"

defineTable :: [(String, TypeQ)] -> String -> String -> [ConName] -> Q [Dec]
defineTable tmap scm tbl derives =
    defineTableFromDB
      connect
      (driverSQLite3 { typeMap = tmap })
      scm tbl derives

module SQLServerTestDataSource (
    connect,
    defineTable
    ) where

import Database.HDBC.ODBC (Connection, connectODBC)
import Database.HDBC.Query.TH (defineTableFromDB)
import Database.HDBC.Schema.Driver (typeMap)
import Database.HDBC.Schema.SQLServer (driverSQLServer)
import Language.Haskell.TH (Q, Dec, TypeQ)
import Language.Haskell.TH.Name.CamelCase (ConName)

{-# ANN module "HLint: ignore Eta reduce" #-}

connect :: IO Connection
connect = connectODBC "DSN=testdb;UID=test;PWD=test"

defineTable :: [(String, TypeQ)] -> String -> String -> [ConName] -> Q [Dec]
defineTable tmap scm tbl derives =
    defineTableFromDB
      connect
      (driverSQLServer { typeMap = tmap })
      scm tbl derives

module MySQLTestDataSource
    (
      connect
    , defineTable
    )
    where

import Language.Haskell.TH                  (Q, Dec, TypeQ)
import Language.Haskell.TH.Name.CamelCase   (ConName)

import Database.HDBC.Query.TH       (defineTableFromDB)
import Database.HDBC.Schema.Driver  (typeMap)
import Database.HDBC.Schema.MySQL   (driverMySQL)
import Database.HDBC.MySQL          ( Connection
                                    , connectMySQL
                                    , MySQLConnectInfo(..)
                                    , defaultMySQLConnectInfo
                                    )

{-# ANN module "HLint: ignore Eta reduce" #-}

connect :: IO Connection
connect = connectMySQL config
    where
        config = defaultMySQLConnectInfo {
                      mysqlUser     = "krdlab"
                    , mysqlPassword = ""
                    , mysqlDatabase = "test"
                    }

defineTable :: [(String, TypeQ)] -> String -> String -> [ConName] -> Q [Dec]
defineTable tmap scm tbl derives =
    defineTableFromDB connect (driverMySQL { typeMap = tmap }) scm tbl derives

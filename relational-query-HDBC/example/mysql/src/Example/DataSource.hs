module Example.DataSource
    (
      config
    , connect
    , defineTable
    )
    where

import Language.Haskell.TH                  (Q, Dec, TypeQ)
import Language.Haskell.TH.Name.CamelCase   (ConName)

import Database.HDBC.Query.TH               (defineTableFromDB)
import Database.HDBC.Schema.Driver          (typeMap)
import Database.HDBC.Schema.MySQL           (driverMySQL)
import Database.HDBC.MySQL                  ( Connection
                                            , connectMySQL
                                            , MySQLConnectInfo(..)
                                            , defaultMySQLConnectInfo
                                            )

{-# ANN module "HLint: ignore Eta reduce" #-}

config :: MySQLConnectInfo
config = defaultMySQLConnectInfo {
              mysqlUser     = "***" -- TODO
            , mysqlPassword = ""
            , mysqlDatabase = "TEST"
            , mysqlHost     = "***" -- TODO: your mysql host
            }

connect :: IO Connection
connect = connectMySQL config

defineTable :: [(String, TypeQ)] -> String -> String -> [ConName] -> Q [Dec]
defineTable tmap scm tbl derives =
    defineTableFromDB connect (driverMySQL { typeMap = tmap }) scm tbl derives

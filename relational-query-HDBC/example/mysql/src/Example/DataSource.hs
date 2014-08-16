module Example.DataSource
    (
      config
    , connect
    , defineTable
    )
    where

import Language.Haskell.TH                  (Q, Dec, TypeQ, runQ, runIO)
import Language.Haskell.TH.Name.CamelCase   (ConName)

import Database.HDBC.Query.TH               (defineTableFromDB)
import Database.HDBC.Schema.Driver          (typeMap)
import Database.HDBC.Schema.MySQL           (driverMySQL)
import Database.HDBC.MySQL                  ( Connection
                                            , connectMySQL
                                            , MySQLConnectInfo(..)
                                            , defaultMySQLConnectInfo
                                            , withRTSSignalsBlocked
                                            )

{-# ANN module "HLint: ignore Eta reduce" #-}

config :: MySQLConnectInfo
config = defaultMySQLConnectInfo {
              mysqlUser     = "hrr-tester"
            , mysqlPassword = ""
            , mysqlDatabase = "TEST"
            , mysqlHost     = "127.0.0.1"
            }

connect :: IO Connection
connect = connectMySQL config

defineTable :: [(String, TypeQ)] -> String -> String -> [ConName] -> Q [Dec]
defineTable tmap scm tbl derives = runIO $ withRTSSignalsBlocked $ runQ $
    defineTableFromDB connect (driverMySQL { typeMap = tmap }) scm tbl derives

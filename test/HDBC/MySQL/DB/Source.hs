module DB.Source (connect, defineTable) where

import Database.HDBC.MySQL                  ( MySQLConnectInfo(..), defaultMySQLConnectInfo
                                            , Connection, connectMySQL
                                            )
import Database.HDBC.Query.TH               (defineTableFromDB)
import Database.HDBC.Schema.Driver          (typeMap)
import Database.HDBC.Schema.MySQL           (driverMySQL)
import Language.Haskell.TH                  (TypeQ, Q, Dec)
import Language.Haskell.TH.Name.CamelCase   (ConName)

-- {-# ANN module "HLint: ignore Eta reduce" #-}

config :: MySQLConnectInfo
config = defaultMySQLConnectInfo
    { mysqlUser     = "hrr-tester"
    , mysqlPassword = ""
    , mysqlDatabase = "TEST"
    , mysqlHost     = "127.0.0.1"
    }

connect :: IO Connection
connect = connectMySQL config

defineTable :: [(String, TypeQ)] -> String -> String -> [ConName] -> Q [Dec]
defineTable tmap =
    defineTableFromDB connect (driverMySQL { typeMap = tmap })


module Example.DataSource
    (
      config
    , connect
    , defineTable
    )
    where

import Language.Haskell.TH         (Q, Dec, TypeQ)
import Language.Haskell.TH.Syntax  (Name)

import Database.HDBC.Query.TH      (defineTableFromDB)
import Database.HDBC.Schema.Driver (typeMap)
import Database.HDBC.Schema.MySQL  (driverMySQL)
import Database.HDBC.MySQL         ( Connection
                                   , connectMySQL
                                   , MySQLConnectInfo(..)
                                   , defaultMySQLConnectInfo
                                   )
import Database.Relational.Schema.MySQLInfo.Config (config)

connConfig :: MySQLConnectInfo
connConfig = defaultMySQLConnectInfo {
              mysqlUser     = "hrr-tester"
            , mysqlPassword = ""
            , mysqlDatabase = "TEST"
            , mysqlHost     = "127.0.0.1"
            }

connect :: IO Connection
connect = connectMySQL connConfig

defineTable :: [(String, TypeQ)] -> String -> String -> [Name] -> Q [Dec]
defineTable tmap = defineTableFromDB connect (driverMySQL { typeMap = tmap })

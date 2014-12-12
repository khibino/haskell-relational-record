{-# LANGUAGE TemplateHaskell #-}

module DataSource (
    connect, convTypes, defineTable
  ) where

import Data.Time (Day, LocalTime)
import Database.HDBC.Query.TH (defineTableFromDB)
import Database.HDBC.Schema.SQLite3 (driverSQLite3)
import Database.HDBC.Schema.Driver (typeMap)
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import Language.Haskell.TH (Q, Dec, TypeQ)
import Language.Haskell.TH.Name.CamelCase (ConName)

connect :: IO Connection
connect = connectSqlite3 "examples.db"

convTypes :: [(String, TypeQ)]
convTypes =
    [ ("float", [t|Double|]) 
    , ("date", [t|Day|])
    , ("datetime", [t|LocalTime|])
    , ("double", [t|Double|])
    , ("varchar", [t|String|])
    ]

defineTable :: [(String, TypeQ)] -> String -> String -> [ConName] -> Q [Dec]
defineTable tmap =
  defineTableFromDB
    connect
    (driverSQLite3 { typeMap = tmap })

{-# LANGUAGE TemplateHaskell #-}

module DataSource (
    connect, convTypes, defineTable
  ) where

import Data.Time (Day, LocalTime)
import Database.HDBC.Query.TH (defineTableFromDB)
import Database.HDBC.Schema.Driver (typeMap)
import Database.HDBC.Schema.SQLite3 (driverSQLite3)
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import Database.Record.TH (derivingShow)
import Language.Haskell.TH (Q, Dec, TypeQ)

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

defineTable :: String -> Q [Dec]
defineTable tableName =
  defineTableFromDB
    connect
    (driverSQLite3 { typeMap = convTypes }) -- overwrite the default type map with yours
    "main" -- schema name, ignored by SQLite
    tableName
    [derivingShow]

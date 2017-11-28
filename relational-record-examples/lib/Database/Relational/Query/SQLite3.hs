{-# LANGUAGE FlexibleContexts #-}

module Database.Relational.Query.SQLite3 (
    module Database.HDBC
  , module Database.HDBC.Query.TH
  , module Database.HDBC.Record
  , module Database.HDBC.Session
  , module Database.HDBC.Sqlite3
  , module Database.Record
  , module Database.Relational
  , runRelation
  , makeRelationalRecord
  ) where

import Language.Haskell.TH (Name, Q, Dec)

import Database.HDBC hiding (execute, finish, run)
import Database.HDBC.Query.TH hiding (makeRelationalRecord)
import Database.HDBC.Record hiding (execute, finish)
import Database.HDBC.Session
import Database.HDBC.Sqlite3
import Database.Record hiding (unique)
import Database.Relational hiding (unique)

runRelation :: (ToSql SqlValue p,
               IConnection conn,
               FromSql SqlValue a) =>
               conn -> Relation p a -> p -> IO [a]
runRelation conn q p = runQuery conn (relationalQuery q) p

makeRelationalRecord :: Name -> Q [Dec]
makeRelationalRecord = makeRelationalRecord' defaultConfig

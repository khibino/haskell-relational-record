{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Relational.Schema.IBMDB2 (
  normalizeColumn, notNull, getType,

  columnsQuerySQL, primaryKeyQuerySQL
  ) where


import Data.Int (Int16, Int32, Int64)
import Data.Char (toLower)
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Data.Time (LocalTime, Day)
import Language.Haskell.TH (TypeQ)

import Database.Record.Instances ()

import Database.Relational.Query.Type (fromRelation)
import Database.Relational.Query
  (Query, PrimeRelation, inner, relation,
   wheres, (.=.), (!), placeholder, asc, value)

import Control.Applicative ((<|>))

import Database.Relational.Schema.DB2Syscat.Columns (Columns, columns)
import qualified Database.Relational.Schema.DB2Syscat.Columns as Columns
import Database.Relational.Schema.DB2Syscat.Tabconst (tabconst)
import qualified Database.Relational.Schema.DB2Syscat.Tabconst as Tabconst
import Database.Relational.Schema.DB2Syscat.Keycoluse (keycoluse)
import qualified Database.Relational.Schema.DB2Syscat.Keycoluse as Keycoluse


mapFromSqlDefault :: Map String TypeQ
mapFromSqlDefault =
  fromList [("VARCHAR",   [t|String|]),
            ("CHAR",      [t|String|]),
            ("CHARACTER", [t|String|]),
            ("TIMESTAMP", [t|LocalTime|]),
            ("DATE",      [t|Day|]),
            ("SMALLINT",  [t|Int16|]),
            ("INTEGER",   [t|Int32|]),
            ("BIGINT",    [t|Int64|]),
            ("BLOB",      [t|String|]),
            ("CLOB",      [t|String|])]

normalizeColumn :: String -> String
normalizeColumn =  map toLower

notNull :: Columns -> Bool
notNull =  (== "N") . Columns.nulls

getType :: Map String TypeQ -> Columns -> Maybe (String, TypeQ)
getType mapFromSql rec = do
  typ <- (Map.lookup key mapFromSql
          <|>
          Map.lookup key mapFromSqlDefault)
  return (normalizeColumn $ Columns.colname rec, mayNull typ)
  where key = Columns.typename rec
        mayNull typ = if notNull rec
                      then typ
                      else [t| Maybe $(typ) |]

columnsRelationFromTable :: PrimeRelation (String, String) Columns
columnsRelationFromTable =  relation $ do
  c <- inner columns
  wheres $ c ! Columns.tabschema' .=. placeholder
  wheres $ c ! Columns.tabname'   .=. placeholder
  asc $ c ! Columns.colno'
  return c

columnsQuerySQL :: Query (String, String) Columns
columnsQuerySQL =  fromRelation columnsRelationFromTable


primaryKeyRelation :: PrimeRelation (String, String) String
primaryKeyRelation =  relation $ do
  cons  <- inner tabconst
  key   <- inner keycoluse
  col   <- inner columns

  wheres $ cons ! Tabconst.tabschema' .=. col ! Columns.tabschema'
  wheres $ cons ! Tabconst.tabname'   .=. col ! Columns.tabname'
  wheres $ key  ! Keycoluse.colname'  .=. col ! Columns.colname'
  wheres $ cons ! Tabconst.constname' .=. key ! Keycoluse.constname'

  wheres $ col  ! Columns.nulls'     .=. value "N"
  wheres $ cons ! Tabconst.type'     .=. value "P"
  wheres $ cons ! Tabconst.enforced' .=. value "Y"

  wheres $ cons ! Tabconst.tabschema' .=. placeholder
  wheres $ cons ! Tabconst.tabname'   .=. placeholder

  return $ key ! Keycoluse.colname'

primaryKeyQuerySQL :: Query (String, String) String
primaryKeyQuerySQL =  fromRelation primaryKeyRelation

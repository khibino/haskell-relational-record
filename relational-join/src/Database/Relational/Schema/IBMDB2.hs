{-# LANGUAGE TemplateHaskell #-}

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
  (Query, Relation, query, relation',
   wheres, (.=.), (!), (><), placeholder, asc, value)

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

columnsRelationFromTable :: Relation (String, String) Columns
columnsRelationFromTable =  relation' $ do
  c <- query columns
  let (schemaParam, schemaPh) = placeholder
      (nameParam  , namePh)   = placeholder
  wheres $ c ! Columns.tabschema' .=. schemaPh
  wheres $ c ! Columns.tabname'   .=. namePh
  asc $ c ! Columns.colno'
  return (schemaParam >< nameParam, c)

columnsQuerySQL :: Query (String, String) Columns
columnsQuerySQL =  fromRelation columnsRelationFromTable


primaryKeyRelation :: Relation (String, String) String
primaryKeyRelation =  relation' $ do
  cons  <- query tabconst
  key   <- query keycoluse
  col   <- query columns

  wheres $ cons ! Tabconst.tabschema' .=. col ! Columns.tabschema'
  wheres $ cons ! Tabconst.tabname'   .=. col ! Columns.tabname'
  wheres $ key  ! Keycoluse.colname'  .=. col ! Columns.colname'
  wheres $ cons ! Tabconst.constname' .=. key ! Keycoluse.constname'

  wheres $ col  ! Columns.nulls'     .=. value "N"
  wheres $ cons ! Tabconst.type'     .=. value "P"
  wheres $ cons ! Tabconst.enforced' .=. value "Y"

  let (schemaParam, schemaPh) = placeholder
      (nameParam  , namePh)   = placeholder

  wheres $ cons ! Tabconst.tabschema' .=. schemaPh
  wheres $ cons ! Tabconst.tabname'   .=. namePh

  return   (schemaParam >< nameParam, key ! Keycoluse.colname')

primaryKeyQuerySQL :: Query (String, String) String
primaryKeyQuerySQL =  fromRelation primaryKeyRelation

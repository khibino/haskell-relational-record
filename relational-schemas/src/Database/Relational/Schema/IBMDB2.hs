{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Database.Relational.Schema.IBMDB2
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module implements queries to get
-- table schema and table constraint informations
-- from system catalog of IBM DB2.
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

import Database.Relational.Query
  (Query, relationalQuery, Relation, query, relation',
   wheres, (.=.), (!), (><), placeholder, asc, value)

import Control.Applicative ((<|>))

import Database.Relational.Schema.DB2Syscat.Columns (Columns, columns)
import qualified Database.Relational.Schema.DB2Syscat.Columns as Columns
import Database.Relational.Schema.DB2Syscat.Tabconst (tabconst)
import qualified Database.Relational.Schema.DB2Syscat.Tabconst as Tabconst
import Database.Relational.Schema.DB2Syscat.Keycoluse (keycoluse)
import qualified Database.Relational.Schema.DB2Syscat.Keycoluse as Keycoluse


-- | Mapping between type in DB2 and Haskell type.
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

-- | Normalize column name string to query DB2 system catalog
normalizeColumn :: String -> String
normalizeColumn =  map toLower

-- | Not-null attribute information of column.
notNull :: Columns -> Bool
notNull =  (== "N") . Columns.nulls

-- | Get column normalized name and column Haskell type.
getType :: Map String TypeQ      -- ^ Type mapping specified by user
        -> Columns               -- ^ Column info in system catalog
        -> Maybe (String, TypeQ) -- ^ Result normalized name and mapped Haskell type
getType mapFromSql rec = do
  typ <- (Map.lookup key mapFromSql
          <|>
          Map.lookup key mapFromSqlDefault)
  return (normalizeColumn $ Columns.colname rec, mayNull typ)
  where key = Columns.typename rec
        mayNull typ = if notNull rec
                      then typ
                      else [t| Maybe $(typ) |]

-- | 'Relation' to query 'Columns' from schema name and table name.
columnsRelationFromTable :: Relation (String, String) Columns
columnsRelationFromTable =  relation' $ do
  c <- query columns
  (schemaP, ()) <- placeholder (\ph -> wheres $ c ! Columns.tabschema' .=. ph)
  (nameP  , ()) <- placeholder (\ph -> wheres $ c ! Columns.tabname'   .=. ph)
  asc $ c ! Columns.colno'
  return (schemaP >< nameP, c)

-- | Phantom typed 'Query' to get 'Columns' from schema name and table name.
columnsQuerySQL :: Query (String, String) Columns
columnsQuerySQL =  relationalQuery columnsRelationFromTable


-- | 'Relation' to query primary key name from schema name and table name.
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

  (schemaP, ()) <- placeholder (\ph -> wheres $ cons ! Tabconst.tabschema' .=. ph)
  (nameP  , ()) <- placeholder (\ph -> wheres $ cons ! Tabconst.tabname'   .=. ph)

  asc  $ key ! Keycoluse.colseq'

  return   (schemaP >< nameP, key ! Keycoluse.colname')

-- | Phantom typed 'Query' to get primary key name from schema name and table name.
primaryKeyQuerySQL :: Query (String, String) String
primaryKeyQuerySQL =  relationalQuery primaryKeyRelation

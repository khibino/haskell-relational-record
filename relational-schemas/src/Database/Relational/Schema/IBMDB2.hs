{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Database.Relational.Schema.IBMDB2
-- Copyright   : 2013-2019 Kei Hibino
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
  module Database.Relational.Schema.IBMDB2.Config,

  normalizeColumn, notNull, getType,

  columnsQuerySQL, primaryKeyQuerySQL
  ) where


import Data.Int (Int16, Int32, Int64)
import Data.Char (toLower)
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Data.Time (LocalTime, Day)
import Language.Haskell.TH (TypeQ)

import Database.Relational
  (Query, relationalQuery, Relation, query, relation',
   wheres, (.=.), (!), asc, value, fst', snd', toFlat)

import Control.Applicative ((<|>))

import Database.Relational.Schema.IBMDB2.Config
import Database.Relational.Schema.IBMDB2.Columns (Columns, columns)
import qualified Database.Relational.Schema.IBMDB2.Columns as Columns
import Database.Relational.Schema.IBMDB2.Tabconst (tabconst)
import qualified Database.Relational.Schema.IBMDB2.Tabconst as Tabconst
import Database.Relational.Schema.IBMDB2.Keycoluse (keycoluse)
import qualified Database.Relational.Schema.IBMDB2.Keycoluse as Keycoluse


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
columnsRelationFromTable =  relation' $ \ph -> do
  c <- query columns
  wheres $ c ! Columns.tabschema' .=. toFlat (ph ! fst')
  wheres $ c ! Columns.tabname'   .=. toFlat (ph ! snd')
  asc $ c ! Columns.colno'
  return c

-- | Phantom typed 'Query' to get 'Columns' from schema name and table name.
columnsQuerySQL :: Query (String, String) Columns
columnsQuerySQL =  relationalQuery columnsRelationFromTable


-- | 'Relation' to query primary key name from schema name and table name.
primaryKeyRelation :: Relation (String, String) String
primaryKeyRelation =  relation' $ \ph -> do
  cons  <- query tabconst
  key   <- query keycoluse
  col   <- query columns

  wheres $ cons ! Tabconst.tabschema' .=. col ! Columns.tabschema'
  wheres $ cons ! Tabconst.tabname'   .=. col ! Columns.tabname'
  wheres $ key  ! Keycoluse.colname'  .=. col ! Columns.colname'
  wheres $ cons ! Tabconst.constname' .=. key ! Keycoluse.constname'

  wheres $ col  ! Columns.nulls'     .=. toFlat (value "N")
  wheres $ cons ! Tabconst.type'     .=. toFlat (value "P")
  wheres $ cons ! Tabconst.enforced' .=. toFlat (value "Y")

  wheres $ cons ! Tabconst.tabschema' .=. toFlat (ph ! fst')
  wheres $ cons ! Tabconst.tabname'   .=. toFlat (ph ! snd')

  asc  $ key ! Keycoluse.colseq'

  return (key ! Keycoluse.colname')

-- | Phantom typed 'Query' to get primary key name from schema name and table name.
primaryKeyQuerySQL :: Query (String, String) String
primaryKeyQuerySQL =  relationalQuery primaryKeyRelation

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

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

import Database.Relational
  (Query, relationalQuery, Relation, query, relation, 
   wheres, (.=.), (!), asc, value, ph)

import Database.Relational.ReboundSyntax hiding ((>>=), (>>))
import qualified Database.Relational.ReboundSyntax as ReboundSyntax
import Database.Relational.ExtensibleRecord (ExRecord, type (>:))

import Control.Applicative ((<|>))

import Database.Relational.Schema.DB2Syscat.Columns (Columns, columns)
import qualified Database.Relational.Schema.DB2Syscat.Columns as Columns
import Database.Relational.Schema.DB2Syscat.Tabconst (tabconst)
import qualified Database.Relational.Schema.DB2Syscat.Tabconst as Tabconst
import Database.Relational.Schema.DB2Syscat.Keycoluse (keycoluse)
import qualified Database.Relational.Schema.DB2Syscat.Keycoluse as Keycoluse

import Prelude hiding ((>>=), (>>))
import qualified Prelude


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
  typ <- Map.lookup key mapFromSql
          <|> Map.lookup key mapFromSqlDefault
  return (normalizeColumn $ Columns.colname rec, mayNull typ)
 where
  key = Columns.typename rec
  mayNull typ = if notNull rec
                then typ
                else [t| Maybe $(typ) |]
  (>>=) :: Monad m => m a -> (a -> m b) -> m b
  (>>=) = (Prelude.>>=)


type NameInSchema = ExRecord
 '[ "tabschema" >: String
  , "tabname" >: String
  ]

-- | 'Relation' to query 'Columns' from schema name and table name.
columnsRelationFromTable :: Relation (ExRecord '[]) NameInSchema () Columns
columnsRelationFromTable =  relation $ do
  c <- query columns
  -- schemaP <- wheres $ ph #tabschema -- c ! Columns.tabschema' .=. ph #tabschema
  wheres $ c ! Columns.tabschema' .=. ph #tabschema
  wheres $ c ! Columns.tabname'   .=. ph #tabname
  asc $ c ! Columns.colno'
  -- ireturn (schemaP >< nameP, c)
  ireturn c
 where
  (>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
  (>>=) = (ReboundSyntax.>>=)
  (>>) :: IxMonad m => m i j a -> m j k b -> m i k b
  (>>) = (ReboundSyntax.>>)

-- | Phantom typed 'Query' to get 'Columns' from schema name and table name.
columnsQuerySQL :: Query () Columns
columnsQuerySQL =  relationalQuery columnsRelationFromTable


-- | 'Relation' to query primary key name from schema name and table name.
primaryKeyRelation :: Relation (ExRecord '[]) NameInSchema () String
primaryKeyRelation =  relation $ do
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

  -- (schemaP, ()) <- placeholder (\ph -> wheres $ cons ! Tabconst.tabschema' .=. ph)
  -- (nameP  , ()) <- placeholder (\ph -> wheres $ cons ! Tabconst.tabname'   .=. ph)
  wheres $ cons ! Tabconst.tabschema' .=. ph #tabschema
  wheres $ cons ! Tabconst.tabname'   .=. ph #tabname

  asc  $ key ! Keycoluse.colseq'

  -- ireturn   (schemaP >< nameP, key ! Keycoluse.colname')
  ireturn $ key ! Keycoluse.colname'
 where
  (>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
  (>>=) = (ReboundSyntax.>>=)
  (>>) :: IxMonad m => m i j a -> m j k b -> m i k b
  (>>) = (ReboundSyntax.>>)

-- | Phantom typed 'Query' to get primary key name from schema name and table name.
primaryKeyQuerySQL :: Query () String
primaryKeyQuerySQL =  relationalQuery primaryKeyRelation

{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Database.Relational.Schema.PostgreSQL
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module implements queries to get
-- table schema and table constraint informations
-- from system catalog of PostgreSQL.
module Database.Relational.Schema.PostgreSQL (
  Column,

  normalizeColumn, notNull, getType,

  columnQuerySQL,
  primaryKeyLengthQuerySQL, primaryKeyQuerySQL
  ) where

import Prelude hiding (or)

import Language.Haskell.TH (TypeQ)

import Data.Int (Int16, Int32, Int64)
import Data.Char (toLower)
import Data.List (foldl1')
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Data.Time
  (DiffTime, NominalDiffTime,
   LocalTime, ZonedTime, Day, TimeOfDay)

import Database.Relational.Query.Type (relationalQuery)
import Database.Relational.Query
  (Query, Relation, query, query', relation', relation, union,
   wheres, (.=.), (.>.), in', values, (!), fst', snd',
   placeholder, asc, value, unsafeProjectSql, (><))

import Database.Relational.Schema.PgCatalog.PgNamespace (pgNamespace)
import qualified Database.Relational.Schema.PgCatalog.PgNamespace as Namespace
import Database.Relational.Schema.PgCatalog.PgClass (pgClass)
import qualified Database.Relational.Schema.PgCatalog.PgClass as Class
import Database.Relational.Schema.PgCatalog.PgConstraint (PgConstraint, pgConstraint)
import qualified Database.Relational.Schema.PgCatalog.PgConstraint as Constraint

import Database.Relational.Schema.PgCatalog.PgAttribute (PgAttribute, pgAttribute)
import qualified Database.Relational.Schema.PgCatalog.PgAttribute as Attr
import Database.Relational.Schema.PgCatalog.PgType (PgType(..), pgType)
import qualified Database.Relational.Schema.PgCatalog.PgType as Type

import Control.Applicative ((<|>))


-- | Mapping between type in PostgreSQL and Haskell type.
mapFromSqlDefault :: Map String TypeQ
mapFromSqlDefault =
  fromList [("bool",         [t| Bool |]),
            ("char",         [t| Char |]),
            ("name",         [t| String |]),
            ("int8",         [t| Int64 |]),
            ("int2",         [t| Int16 |]),
            ("int4",         [t| Int32 |]),
            -- ("regproc",      [t| Int32 |]),
            ("text",         [t| String |]),
            ("oid",          [t| Int32 |]),
            -- ("pg_node_tree", [t| String |]),
            ("float4",       [t| Float |]),
            ("float8",       [t| Double |]),
            ("abstime",      [t| LocalTime |]),
            ("reltime",      [t| NominalDiffTime |]),
            ("tinterval",    [t| DiffTime |]),
            -- ("money",        [t| Decimal |]),
            ("bpchar",       [t| String |]),
            ("varchar",      [t| String |]),
            ("date",         [t| Day |]),
            ("time",         [t| TimeOfDay |]),
            ("timestamp",    [t| LocalTime |]),
            ("timestamptz",  [t| ZonedTime |]),
            ("interval",     [t| DiffTime |]),
            ("timetz",       [t| ZonedTime |])

            -- ("bit", [t|  |]),
            -- ("varbit", [t|  |]),
            -- ("numeric", [t| Decimal |])
           ]

-- | Normalize column name string to query PostgreSQL system catalog.
normalizeColumn :: String -> String
normalizeColumn =  map toLower

-- | Type to represent Column information.
type Column = (PgAttribute, PgType)

-- | Not-null attribute information of column.
notNull :: Column -> Bool
notNull =  Attr.attnotnull . fst

-- | Get column normalized name and column Haskell type.
getType :: Map String TypeQ      -- ^ Type mapping specified by user
        -> Column                -- ^ Column info in system catalog
        -> Maybe (String, TypeQ) -- ^ Result normalized name and mapped Haskell type
getType mapFromSql column@(pgAttr, pgTyp) = do
  typ <- (Map.lookup key mapFromSql
          <|>
          Map.lookup key mapFromSqlDefault)
  return (normalizeColumn $ Attr.attname pgAttr,
          mayNull typ)
  where key = Type.typname pgTyp
        mayNull typ = if notNull column
                      then typ
                      else [t| Maybe $typ |]

-- | 'Relation' to query PostgreSQL relation oid from schema name and table name.
relOidRelation :: Relation (String, String) Int32
relOidRelation = relation' $ do
  nsp <- query pgNamespace
  cls <- query pgClass

  wheres $ cls ! Class.relnamespace' .=. nsp ! Namespace.oid'
  (nspP, ()) <- placeholder (\ph -> wheres $ nsp ! Namespace.nspname'  .=. ph)
  (relP, ()) <- placeholder (\ph -> wheres $ cls ! Class.relname'      .=. ph)

  return   (nspP >< relP, cls ! Class.oid')

-- | 'Relation' to query column attribute from schema name and table name.
attributeRelation :: Relation (String, String) PgAttribute
attributeRelation =  relation' $ do
  (ph, reloid) <- query' relOidRelation
  att          <- query  pgAttribute

  wheres $ att ! Attr.attrelid' .=. reloid
  wheres $ att ! Attr.attnum'   .>. value 0

  return   (ph, att)

-- | 'Relation' to query 'Column' from schema name and table name.
columnRelation :: Relation (String, String) Column
columnRelation = relation' $ do
  (ph, att) <- query' attributeRelation
  typ       <- query  pgType

  wheres $ att ! Attr.atttypid'    .=. typ ! Type.oid'
  wheres $ typ ! Type.typtype'     .=. value 'b'  -- 'b': base type only

  wheres $ typ ! Type.typcategory' `in'` values [ 'B' -- Boolean types
                                                , 'D' -- Date/time types
                                                , 'N' -- Numeric types
                                                , 'S' -- String types
                                                , 'T' -- typespan types
                                                ]

  asc $ att ! Attr.attnum'

  return (ph, att >< typ)

-- | Phantom typed 'Query' to get 'Column' from schema name and table name.
columnQuerySQL :: Query (String, String) Column
columnQuerySQL =  relationalQuery columnRelation

-- | 'Relation' to query primary key length from schema name and table name.
primaryKeyLengthRelation :: Relation (String, String) Int32
primaryKeyLengthRelation =  relation' $ do
  (ph, reloid) <- query' relOidRelation
  con       <- query  pgConstraint

  wheres $ con ! Constraint.conrelid' .=. reloid
  wheres $ con ! Constraint.contype'  .=. value 'p'  -- 'p': primary key constraint type

  return (ph, unsafeProjectSql "array_length (conkey, 1)")

-- | Phantom typed 'Query' to get primary key length from schema name and table name.
primaryKeyLengthQuerySQL :: Query (String, String) Int32
primaryKeyLengthQuerySQL =  relationalQuery primaryKeyLengthRelation

-- | One column which is nth column of composite primary key.
constraintColRelation :: Int32 -> Relation () (PgConstraint, (Int16, Int32))
constraintColRelation i = relation $ do
  con <- query pgConstraint

  return $ con >< (unsafeProjectSql ("conkey[" ++ show i ++ "]") >< value i)

-- | Make composite primary key relation from primary key length.
constraintColExpandRelation :: Int32 -> Relation () (PgConstraint, (Int16, Int32))
constraintColExpandRelation n =
  foldl1' union [constraintColRelation i | i <- [1..n] ]

-- | 'Relation' to query primary key name from schema name and table name.
primaryKeyRelation :: Int32 -> Relation (String, String) String
primaryKeyRelation n = relation' $ do
  (ph, att) <- query' attributeRelation
  conEx     <- query  (constraintColExpandRelation n)

  let con = conEx ! fst'
      col' = conEx ! snd'
      keyIx = col' ! fst'
      keyN  = col' ! snd'

  wheres $ con ! Constraint.conrelid' .=. att ! Attr.attrelid'
  wheres $ keyIx .=. att ! Attr.attnum'
  wheres $ con ! Constraint.contype'  .=. value 'p'  -- 'p': primary key constraint type

  asc  $ keyN

  return (ph, att ! Attr.attname')

-- | Phantom typed 'Query' to get primary key name from schema name and table name.
primaryKeyQuerySQL :: Int32 -> Query (String, String) String
primaryKeyQuerySQL =  relationalQuery . primaryKeyRelation

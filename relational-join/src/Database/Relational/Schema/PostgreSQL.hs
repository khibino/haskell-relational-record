{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Relational.Schema.PostgreSQL (
  Column,

  normalizeColumn, notNull, getType,

  columnQuerySQL, primaryKeyQuerySQL
  ) where

import Prelude hiding (or)

import Language.Haskell.TH (TypeQ)

import Data.Int (Int16, Int32, Int64)
import Data.Char (toLower)
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Data.Time
  (DiffTime, NominalDiffTime,
   LocalTime, ZonedTime, Day, TimeOfDay)

import Database.Record.Instances ()

import Database.Relational.Query.Type (fromRelation)
import Database.Relational.Query
  (Query, PrimeRelation, inner, relation, inner', relation', expr,
   wheres, (.=.), (.>.), in', values, (!),
   placeholder, asc, value, unsafeSqlValue, (>*<))

import Database.Relational.Schema.PgCatalog.PgNamespace (pgNamespace)
import qualified Database.Relational.Schema.PgCatalog.PgNamespace as Namespace
import Database.Relational.Schema.PgCatalog.PgClass (pgClass)
import qualified Database.Relational.Schema.PgCatalog.PgClass as Class
import Database.Relational.Schema.PgCatalog.PgConstraint (pgConstraint)
import qualified Database.Relational.Schema.PgCatalog.PgConstraint as Constraint

import Database.Relational.Schema.PgCatalog.PgAttribute (PgAttribute, pgAttribute)
import qualified Database.Relational.Schema.PgCatalog.PgAttribute as Attr
import Database.Relational.Schema.PgCatalog.PgType (PgType(..), pgType)
import qualified Database.Relational.Schema.PgCatalog.PgType as Type

import Control.Applicative ((<|>))


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

normalizeColumn :: String -> String
normalizeColumn =  map toLower

type Column = (PgAttribute, PgType)

notNull :: Column -> Bool
notNull =  Attr.attnotnull . fst

getType :: Map String TypeQ -> Column -> Maybe (String, TypeQ)
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

relOidRelation :: PrimeRelation (String, String) Int32
relOidRelation = relation $ do
  nsp <- inner pgNamespace
  cls <- inner pgClass

  wheres $ cls ! Class.relnamespace' .=. nsp ! Namespace.oid'
  wheres $ nsp ! Namespace.nspname'  .=. placeholder
  wheres $ cls ! Class.relname'      .=. placeholder

  return $ cls ! Class.oid'

attributeRelation :: PrimeRelation (String, String) PgAttribute
attributeRelation =  relation' $ do
  (ph, reloid) <- inner' relOidRelation
  att          <- inner  pgAttribute

  wheres $ att ! Attr.attrelid' .=. expr reloid
  wheres $ att ! Attr.attnum'   .>. value 0

  return   (ph, att)

columnRelation :: PrimeRelation (String, String) Column
columnRelation = relation' $ do
  (ph, att) <- inner' attributeRelation
  typ       <- inner  pgType

  wheres $ att ! Attr.atttypid'    .=. typ ! Type.oid'
  wheres $ typ ! Type.typtype'     .=. value 'b'  -- 'b': base type only

  wheres $ typ ! Type.typcategory' `in'` values [ 'B' -- Boolean types
                                                , 'D' -- Date/time types
                                                , 'N' -- Numeric types
                                                , 'S' -- String types
                                                , 'T' -- typespan types
                                                ]
 
  asc $ att ! Attr.attnum'

  return (ph, att >*< typ)

columnQuerySQL :: Query (String, String) Column
columnQuerySQL =  fromRelation columnRelation

primaryKeyRelation :: PrimeRelation (String, String) String
primaryKeyRelation = relation' $ do
  (ph, att) <- inner' attributeRelation
  con       <- inner pgConstraint

  wheres $ con ! Constraint.conrelid' .=. att ! Attr.attrelid'
  wheres $ unsafeSqlValue "conkey[1]" .=. att ! Attr.attnum'
  wheres $ att ! Attr.attnotnull'
  wheres $ con ! Constraint.contype'  .=. value 'p'  -- 'p': primary key constraint type
  wheres $ unsafeSqlValue "array_length (conkey, 1)" .=. value (1 :: Int32)

  return (ph, att ! Attr.attname')

primaryKeyQuerySQL :: Query (String, String) String
primaryKeyQuerySQL =  fromRelation primaryKeyRelation

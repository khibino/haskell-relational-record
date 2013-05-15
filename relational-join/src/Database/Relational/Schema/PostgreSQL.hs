{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Relational.Schema.PostgreSQL (
  Column,

  normalizeColumn, notNull, getType,

  columnQuerySQL, primaryKeyQuerySQL
  ) where

import Language.Haskell.TH (TypeQ)

import Data.Int (Int16, Int32, Int64)
import Data.Char (toLower)
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Data.Time
  (DiffTime, NominalDiffTime,
   LocalTime, ZonedTime, Day, TimeOfDay)

import qualified Database.Relational.Query.Table as Table
import Database.Relational.Query.Type (unsafeTypedQuery)
import Database.Relational.Query (Query(untypeQuery))

import Database.Relational.Schema.PgCatalog.PgAttribute (PgAttribute, tableOfPgAttribute)
import qualified Database.Relational.Schema.PgCatalog.PgAttribute as Attr
import Database.Relational.Schema.PgCatalog.PgType (PgType(..), tableOfPgType)
import qualified Database.Relational.Schema.PgCatalog.PgType as Type

import Language.SQL.Keyword (Keyword(..), (<.>), (.=.))
import qualified Language.SQL.Keyword as SQL

import Control.Applicative ((<|>))


mapFromSqlDefault :: Map String TypeQ
mapFromSqlDefault =
  fromList [("bool",         [t| Bool |]),
            ("char",         [t| String |]),
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
getType mapFromSql column@(pgAttr, pgType) = do
  typ <- (Map.lookup key mapFromSql
          <|>
          Map.lookup key mapFromSqlDefault)
  return (normalizeColumn $ Attr.attname pgAttr,
          mayNull typ)
  where key = Type.typname pgType
        mayNull typ = if notNull column
                      then typ
                      else [t| Maybe $typ |]

pgCatalog :: SQL.Keyword
pgCatalog =  "PG_CATALOG"

relOidQuerySQL :: Query (String, String) (Int32)
relOidQuerySQL =
  unsafeTypedQuery .
  SQL.unwordsSQL
  $ [SELECT,
     "rel" <.> "oid", AS, "rel_object_id",
     FROM,
     pgCatalog <.> "pg_namespace", AS, "nsp", ",",
     pgCatalog <.> "pg_class",     AS, "rel",
     WHERE,
     "rel" <.> "relnamespace" .=. "nsp" <.> "oid",  AND,

     "nspname" .=. "?", AND, "relname" .=. "?"
    ]

attributeQuerySQL :: Query (String, String) PgAttribute
attributeQuerySQL =
  unsafeTypedQuery .
  SQL.unwordsSQL
  $ [SELECT,
     map (("att" <.>) . SQL.word) (Table.columns tableOfPgAttribute) `SQL.sepBy` ", ",
     FROM,
     "(", SQL.word $ untypeQuery relOidQuerySQL, ")", AS, "rel", ",",
     SQL.word (Table.name tableOfPgAttribute),        AS, "att",
     WHERE,
     "attrelid" .=. "rel_object_id", AND,
     "attnum", ">", "0" -- attnum of normal attributes begins from 1
    ]

columnQuerySQL :: Query (String, String) Column
columnQuerySQL =
  unsafeTypedQuery .
  SQL.unwordsSQL
  $ [SELECT,
     (map (("att" <.>) . SQL.word) (Table.columns tableOfPgAttribute) ++
      map (("typ"  <.>) . SQL.word) (Table.columns tableOfPgType))
     `SQL.sepBy` ", ",
     FROM,
     "(", SQL.word $ untypeQuery attributeQuerySQL, ")", AS, "att", ",",
     SQL.word (Table.name tableOfPgType),                AS, "typ",
     WHERE,
     "atttypid" .=. "typ" <.> "oid", AND,
     "typ" <.> "typtype" .=. "'b'",  AND,
     "(",
     "typcategory = 'B'", OR,
     "typcategory = 'D'", OR,
     "typcategory = 'N'", OR,
     "typcategory = 'S'", OR,
     "typcategory = 'T'",
     ")" ]

primaryKeyQuerySQL :: Query (String, String) String
primaryKeyQuerySQL =
  unsafeTypedQuery .
  SQL.unwordsSQL
  $ [SELECT, "attname", FROM,
     "(", SQL.word $ untypeQuery attributeQuerySQL, ")", AS, "att", ",",
     pgCatalog <.> "pg_constraint",                      AS, "con",
     WHERE,
     "conrelid"  .=. "attrelid", AND,
     "conkey[1]" .=. "attnum",   AND,

     "attnotnull" .=. "TRUE",    AND,
     "contype" .=. "'p'",        AND,
     "array_length (conkey, 1)" .=. "1"]

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Database.HDBC.Schema.PgCatalog.PgAttribute
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.Relational.Schema.PgCatalog.PgAttribute where

import GHC.Generics (Generic)
import Data.Int (Int16, Int32)
import Database.Relational.TH (defineTableTypesAndRecord)

import Database.Relational.Schema.PgCatalog.Config (config)


$(defineTableTypesAndRecord config
  "PG_CATALOG" "pg_attribute"

  [
-- Table "pg_catalog.pg_attribute"
--     Column     |   Type    | Modifiers
-- ---------------+-----------+-----------
--  attrelid      | oid       | not null
    ("attrelid"     , [t|Int32|]),
--  attname       | name      | not null
    ("attname"      , [t|String|]),
--  atttypid      | oid       | not null
    ("atttypid"     , [t|Int32|]),
--  attstattarget | integer   | not null
    ("attstattarget", [t|Int32|]),
--  attlen        | smallint  | not null
    ("attlen"       , [t|Int16|]),
--  attnum        | smallint  | not null
    ("attnum"      , [t|Int16|]),
--  attndims      | integer   | not null
    ("attndims"    , [t|Int32|]),
--  attcacheoff   | integer   | not null
    ("attcacheoff" , [t|Int32|]),
--  atttypmod     | integer   | not null
    ("atttypmod"   , [t|Int32|]),
--  attbyval      | boolean   | not null
    ("attbyval"    , [t|Bool|]),
--  attstorage    | "char"    | not null
    ("attstorage"  , [t|Char|]),
--  attalign      | "char"    | not null
    ("attalign"    , [t|Char|]),
--  attnotnull    | boolean   | not null
    ("attnotnull"  , [t|Bool|]),
--  atthasdef     | boolean   | not null
    ("atthasdef"   , [t|Bool|]),
--  attisdropped  | boolean   | not null
    ("attisdropped", [t|Bool|]),
--  attislocal    | boolean   | not null
    ("attislocal"  , [t|Bool|]),
--  attinhcount   | integer   | not null
    ("attinhcount" , [t|Int32|]),
--  attcollation  | oid       | not null
    ("attcollation", [t|Int32|])
--  attacl        | aclitem[] |
    -- ("attacl"      , [t|String|]),
--  attoptions    | text[]    |
    -- ("attoptions"  , [t|String|])
  ]
  [''Show, ''Generic])

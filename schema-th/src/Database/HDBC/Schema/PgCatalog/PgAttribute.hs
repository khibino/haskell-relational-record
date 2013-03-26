{-# LANGUAGE TemplateHaskell #-}

module Database.HDBC.Schema.PgCatalog.PgAttribute where

import Database.HDBC.TH (derivingShow)
import qualified Database.HDBC.TH as Base

$(Base.defineTableDefault
  "PG_CATALOG" "pg_attribute"

  [
-- Table "pg_catalog.pg_attribute"
--     Column     |   Type    | Modifiers
-- ---------------+-----------+-----------
--  attrelid      | oid       | not null
    ("attrelid"     , [t|Int|]),
--  attname       | name      | not null
    ("attname"      , [t|String|]),
--  atttypid      | oid       | not null
    ("atttypid"     , [t|Int|]),
--  attstattarget | integer   | not null
    ("attstattarget", [t|Int|]),
--  attlen        | smallint  | not null
    ("attlen"       , [t|Int|]),
--  attnum        | smallint  | not null
    ("attnum"      , [t|Int|]),
--  attndims      | integer   | not null
    ("attndims"    , [t|Int|]),
--  attcacheoff   | integer   | not null
    ("attcacheoff" , [t|Int|]),
--  atttypmod     | integer   | not null
    ("atttypmod"   , [t|Int|]),
--  attbyval      | boolean   | not null
    ("attbyval"    , [t|Bool|]),
--  attstorage    | "char"    | not null
    ("attstorage"  , [t|String|]),
--  attalign      | "char"    | not null
    ("attalign"    , [t|String|]),
--  attnotnull    | boolean   | not null
    ("attnotnull"  , [t|Bool|]),
--  atthasdef     | boolean   | not null
    ("atthasdef"   , [t|Bool|]),
--  attisdropped  | boolean   | not null
    ("attisdropped", [t|Bool|]),
--  attislocal    | boolean   | not null
    ("attislocal"  , [t|Bool|]),
--  attinhcount   | integer   | not null
    ("attinhcount" , [t|Int|]),
--  attcollation  | oid       | not null
    ("attcollation", [t|Int|])
--  attacl        | aclitem[] |
    -- ("attacl"      , [t|String|]),
--  attoptions    | text[]    |
    -- ("attoptions"  , [t|String|])
  ]
  [derivingShow])

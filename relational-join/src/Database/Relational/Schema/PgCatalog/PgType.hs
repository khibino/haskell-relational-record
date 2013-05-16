{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Database.Relational.Schema.PgCatalog.PgType
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.Relational.Schema.PgCatalog.PgType where

import Data.Int (Int16, Int32)

import Database.Record.TH (derivingShow)
import Database.Relational.Query.TH (defineTableTypesAndRecordDefault)

$(defineTableTypesAndRecordDefault
  "PG_CATALOG" "pg_type"

  [
    ("oid", [t|Int32|]),
-- Table "pg_catalog.pg_type"
--      Column     |     Type     | Modifiers
-- ----------------+--------------+-----------
--  typname        | name         | not null
    ("typname", [t|String|]),
--  typnamespace   | oid          | not null
    ("typnamespace", [t|Int32|]),
--  typowner       | oid          | not null
    ("typowner", [t|Int32|]),
--  typlen         | smallint     | not null
    ("typlen", [t|Int16|]),
--  typbyval       | boolean      | not null
    ("typbyval", [t|Bool|]),
--  typtype        | "char"       | not null
    ("typtype", [t|Char|]),
--  typcategory    | "char"       | not null
    ("typcategory", [t|Char|]),
--  typispreferred | boolean      | not null
    ("typispreferred", [t|Bool|]),
--  typisdefined   | boolean      | not null
    ("typisdefined", [t|Bool|]),
--  typdelim       | "char"       | not null
    ("typdelim", [t|Char|]),
--  typrelid       | oid          | not null
    ("typrelid", [t|Int32|]),
--  typelem        | oid          | not null
    ("typelem", [t|Int32|]),
--  typarray       | oid          | not null
    ("typarray", [t|Int32|]),
--  typinput       | regproc      | not null
    -- ("typinput", [t||]),
--  typoutput      | regproc      | not null
    -- ("typoutput", [t||]),
--  typreceive     | regproc      | not null
    -- ("typreceive", [t||]),
--  typsend        | regproc      | not null
    -- ("typsend", [t||]),
--  typmodin       | regproc      | not null
    -- ("typmodin", [t||]),
--  typmodout      | regproc      | not null
    -- ("typmodout", [t||]),
--  typanalyze     | regproc      | not null
    -- ("typanalyze", [t||]),
--  typalign       | "char"       | not null
    ("typalign", [t|Char|]),
--  typstorage     | "char"       | not null
    ("typstorage", [t|Char|]),
--  typnotnull     | boolean      | not null
    ("typnotnull", [t|Bool|]),
--  typbasetype    | oid          | not null
    ("typbasetype", [t|Int32|]),
--  typtypmod      | integer      | not null
    ("typtypmod", [t|Int32|]),
--  typndims       | integer      | not null
    ("typndims", [t|Int32|]),
--  typcollation   | oid          | not null
    ("typcollation", [t|Int32|]),
--  typdefaultbin  | pg_node_tree |
    -- ("typdefaultbin", [t||]),
--  typdefault     | text         |
    ("typdefault", [t|Maybe String|])
  ]
  [derivingShow])

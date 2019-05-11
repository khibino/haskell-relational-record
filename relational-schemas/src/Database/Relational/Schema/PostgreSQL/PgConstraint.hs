{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Database.Relational.Schema.PostgreSQL.PgConstraint
-- Copyright   : 2013-2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.Relational.Schema.PostgreSQL.PgConstraint where

import GHC.Generics (Generic)
import Data.Int (Int32)
import Database.Relational.TH (defineTableTypesAndRecord)

import Database.Relational.Schema.PostgreSQL.Config (config)


$(defineTableTypesAndRecord config
  "PG_CATALOG" "pg_constraint"
  [ -- ("oid"    , [t| Int32 |]),
 -- conname       | name         | not null
 -- connamespace  | oid          | not null
 -- contype       | "char"       | not null
    ("contype",   [t| Char |]),
 -- condeferrable | boolean      | not null
 -- condeferred   | boolean      | not null
 -- convalidated  | boolean      | not null
 -- conrelid      | oid          | not null
    ("conrelid",  [t| Int32 |])
 -- contypid      | oid          | not null
 -- conindid      | oid          | not null
 -- confrelid     | oid          | not null
 -- confupdtype   | "char"       | not null
 -- confdeltype   | "char"       | not null
 -- confmatchtype | "char"       | not null
 -- conislocal    | boolean      | not null
 -- coninhcount   | integer      | not null
 -- conkey        | smallint[]   |
    -- ("conkey",  ???),
 -- confkey       | smallint[]   |
 -- conpfeqop     | oid[]        |
 -- conppeqop     | oid[]        |
 -- conffeqop     | oid[]        |
 -- conexclop     | oid[]        |
 -- conbin        | pg_node_tree |
 -- consrc        | text         |
  ]
  [''Show, ''Generic])

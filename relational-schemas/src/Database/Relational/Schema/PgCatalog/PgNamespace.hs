{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Database.Relational.Schema.PgCatalog.PgNamespace
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.Relational.Schema.PgCatalog.PgNamespace where

import Data.Int (Int32)

import Database.Record.TH (derivingShow)
import Database.Relational.Query.TH (defineTableTypesAndRecordDefault)


$(defineTableTypesAndRecordDefault
  "PG_CATALOG" "pg_namespace"
  [("oid"    , [t| Int32 |]),
 -- nspname  | name      | not null
   ("nspname", [t| String |])
 -- nspowner | oid       | not null
 -- nspacl   | aclitem[] |
  ]
  [derivingShow])

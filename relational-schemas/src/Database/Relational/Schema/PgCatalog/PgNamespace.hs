{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Database.Relational.Schema.PgCatalog.PgNamespace
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.Relational.Schema.PgCatalog.PgNamespace where

import GHC.Generics (Generic)
import Data.Int (Int32)
import Database.Relational.Query.TH (defineTableTypesAndRecord)

import Database.Relational.Schema.PgCatalog.Config (config)


$(defineTableTypesAndRecord config
  "PG_CATALOG" "pg_namespace"
  [("oid"    , [t| Int32 |]),
 -- nspname  | name      | not null
   ("nspname", [t| String |])
 -- nspowner | oid       | not null
 -- nspacl   | aclitem[] |
  ]
  [''Show, ''Generic])

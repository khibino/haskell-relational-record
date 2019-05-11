{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Database.Relational.Schema.PostgreSQL.PgNamespace
-- Copyright   : 2013-2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.Relational.Schema.PostgreSQL.PgNamespace where

import GHC.Generics (Generic)
import Data.Int (Int32)
import Database.Relational.TH (defineTableTypesAndRecord)

import Database.Relational.Schema.PostgreSQL.Config (config)


$(defineTableTypesAndRecord config
  "PG_CATALOG" "pg_namespace"
  [("oid"    , [t| Int32 |]),
 -- nspname  | name      | not null
   ("nspname", [t| String |])
 -- nspowner | oid       | not null
 -- nspacl   | aclitem[] |
  ]
  [''Show, ''Generic])

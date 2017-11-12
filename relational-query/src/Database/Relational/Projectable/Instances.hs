{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Database.Relational.Projectable.Instances
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides instances between projected terms and SQL terms.
module Database.Relational.Projectable.Instances () where

import Database.Relational.Internal.ContextType
  (Flat, Aggregated, OverWindow)
import qualified Database.Relational.Record as Record
import Database.Relational.Projectable.Unsafe
  (SqlContext (..), OperatorContext)


-- | Unsafely make 'Record' from SQL terms.
instance SqlContext Flat where
  unsafeProjectSqlTerms = Record.unsafeFromSqlTerms

-- | Unsafely make 'Record' from SQL terms.
instance SqlContext Aggregated where
  unsafeProjectSqlTerms = Record.unsafeFromSqlTerms

-- | Unsafely make 'Record' from SQL terms.
instance SqlContext OverWindow where
  unsafeProjectSqlTerms = Record.unsafeFromSqlTerms

instance OperatorContext Flat
instance OperatorContext Aggregated

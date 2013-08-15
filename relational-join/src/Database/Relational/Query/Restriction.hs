-- |
-- Module      : Database.Relational.Query.Relation
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines simple restriction
-- for update and delete statement.
module Database.Relational.Query.Restriction (
  -- * Object to express simple restriction.
  Restriction, restriction, restriction',

  -- * Generate SQL from simple restriction.
  sqlWhereFromRestriction
  ) where

import Database.Relational.Query.Table (Table)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Projectable (PlaceHolders)

import Database.Relational.Query.Monad.Trans.Restrict (prependWhere)
import Database.Relational.Query.Monad.Restrict (Restrict, expandPrepend)

-- | Restriction type with place-holder parameter 'p' and projection record type 'r'.
newtype Restriction p r = Restriction (Projection r -> Restrict ())

-- | Finalize 'Restrict' monad and generate 'Restriction'.
restriction :: (Projection r -> Restrict ()) -> Restriction () r
restriction =  Restriction

-- | Finalize 'Restrict' monad and generate 'Restriction' with place-holder parameter 'p'
restriction' :: (Projection r -> Restrict (PlaceHolders p)) -> Restriction p r
restriction'  = Restriction . (fmap (const ()) .)

-- | SQL WHERE clause 'ShowS' string from 'Restriction'.
sqlWhereFromRestriction :: Table r -> Restriction p r -> ShowS
sqlWhereFromRestriction tbl (Restriction q) = prependWhere aw
  where (_ph, aw) = expandPrepend (q $ Projection.unsafeFromTable tbl)

-- |
-- Module      : Database.Relational.Projectable.Unsafe
-- Copyright   : 2017-2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides unsafe interfaces between projected terms and SQL terms.
module Database.Relational.Projectable.Unsafe (
  SqlContext (..), OperatorContext, AggregatedContext,
  PlaceHolders (..)
  ) where

import Database.Relational.Internal.String (StringSQL)
import Database.Relational.Typed.Record (Record)

-- | Interface to project SQL terms unsafely.
class SqlContext c where
  -- | Unsafely project from SQL expression terms.
  unsafeProjectSqlTerms :: [StringSQL]
                        -> Record c t

-- | Constraint to restrict context of full SQL expressions.
--   For example, the expression at the left of OVER clause
--   is not allowed using full SQL expression.
class SqlContext c => OperatorContext c

-- | Constraint to restrict context of aggregated SQL context.
class AggregatedContext ac


-- | Placeholder parameter type which has real parameter type argument 'p'.
data PlaceHolders p = PlaceHolders

-- |
-- Module      : Database.Relational.Query.Internal.GroupingSQL
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides grouping-sets structure of SQL syntax tree.
module Database.Relational.Query.Internal.GroupingSQL (
  AggregateColumnRef,
  AggregateBitKey (..), AggregateSet (..), AggregateElem (..),
  ) where

import Database.Relational.Query.Internal.SQL (ColumnSQL)


-- | Type for group-by term
type AggregateColumnRef = ColumnSQL

-- | Type for group key.
newtype AggregateBitKey = AggregateBitKey [AggregateColumnRef] deriving Show

-- | Type for grouping set
newtype AggregateSet = AggregateSet [AggregateElem] deriving Show

-- | Type for group-by tree
data AggregateElem = ColumnRef AggregateColumnRef
                   | Rollup [AggregateBitKey]
                   | Cube   [AggregateBitKey]
                   | GroupingSets [AggregateSet]
                   deriving Show

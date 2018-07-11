-- |
-- Module      : Database.Relational.SqlSyntax.Aggregate
-- Copyright   : 2013-2018 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides grouping-sets structure of SQL syntax tree.
module Database.Relational.SqlSyntax.Aggregate (
  aggregateColumnRef,
  aggregatePowerKey, aggregateGroupingSet,
  aggregateRollup, aggregateCube, aggregateSets,

  aggregateKeyRecord, aggregateKeyElement, unsafeAggregateKey,
  ) where

import Database.Relational.SqlSyntax.Types
  (AggregateBitKey (..), AggregateSet (..), AggregateElem (..),
   AggregateColumnRef, AggregateKey (..), )


-- | Single term aggregation element.
aggregateColumnRef :: AggregateColumnRef -> AggregateElem
aggregateColumnRef =  ColumnRef

-- | Key of aggregation power set.
aggregatePowerKey :: [AggregateColumnRef] -> AggregateBitKey
aggregatePowerKey =  AggregateBitKey

-- | Single grouping set.
aggregateGroupingSet :: [AggregateElem] -> AggregateSet
aggregateGroupingSet =  AggregateSet

-- | Rollup aggregation element.
aggregateRollup :: [AggregateBitKey] -> AggregateElem
aggregateRollup =  Rollup

-- | Cube aggregation element.
aggregateCube :: [AggregateBitKey] -> AggregateElem
aggregateCube =  Cube

-- | Grouping sets aggregation.
aggregateSets :: [AggregateSet] -> AggregateElem
aggregateSets =  GroupingSets

-- | Extract typed record from 'AggregateKey'.
aggregateKeyRecord :: AggregateKey a -> a
aggregateKeyRecord (AggregateKey (p, _c)) = p

-- | Extract untyped term from 'AggregateKey'.
aggregateKeyElement :: AggregateKey a -> AggregateElem
aggregateKeyElement (AggregateKey (_p, c)) = c

-- | Unsafely bind typed-record and untyped-term into 'AggregateKey'.
unsafeAggregateKey :: (a, AggregateElem) -> AggregateKey a
unsafeAggregateKey = AggregateKey

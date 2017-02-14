{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Database.Relational.Query.Component
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides untyped components for query.
module Database.Relational.Query.Component
       ( -- * Type for column SQL string

         -- deprecated interfaces
         ColumnSQL, columnSQL, columnSQL', showsColumnSQL,

         -- * Configuration type for query
         module Database.Relational.Query.Internal.Config,

         -- * Duplication attribute
         -- re-export
         Duplication (..),

         showsDuplication,

         -- * Types for aggregation
         -- re-export
         AggregateColumnRef,

         -- re-export
         AggregateBitKey, AggregateSet, AggregateElem,

         aggregateColumnRef, aggregateEmpty,
         aggregatePowerKey, aggregateGroupingSet,
         aggregateRollup, aggregateCube, aggregateSets,

         composeGroupBy, composePartitionBy,

         -- re-export
         AggregateKey,

         aggregateKeyProjection, aggregateKeyElement, unsafeAggregateKey,

         -- * Types for ordering
         -- re-export
         Order (..), OrderColumn, OrderingTerm,

         composeOrderBy,

         -- deprecated interfaces
         OrderingTerms,

         -- * Types for assignments
         -- re-export
         AssignColumn, AssignTerm, Assignment,

         Assignments,

         composeSets, composeValues,

         -- * Compose window clause
         composeOver,
       ) where

import Data.Monoid (Monoid (..), (<>))

import Language.SQL.Keyword (Keyword(..), (|*|), (.=.))
import qualified Language.SQL.Keyword as SQL

import Database.Relational.Query.Internal.Config
  (NameConfig (..),
   ProductUnitSupport (..), SchemaNameMode (..), IdentifierQuotation (..),
   Config (..), defaultConfig,)
import Database.Relational.Query.Internal.SQL (StringSQL, rowConsStringSQL)
import qualified Database.Relational.Query.Internal.SQL as Internal
import Database.Relational.Query.Internal.BaseSQL
  (Duplication (..),
   Order (..), OrderColumn, OrderingTerm,
   AssignColumn, AssignTerm, Assignment,)
import Database.Relational.Query.Internal.GroupingSQL
  (AggregateColumnRef,
   AggregateBitKey (..), AggregateSet (..), AggregateElem (..), AggregateKey (..), )


{-# DEPRECATED ColumnSQL, columnSQL, columnSQL', showsColumnSQL "prepare to drop public interface. internally use Database.Relational.Query.Internal.SQL.*" #-}
-- | Column SQL string type
type ColumnSQL = Internal.ColumnSQL

-- | 'ColumnSQL' from string
columnSQL :: String -> ColumnSQL
columnSQL = Internal.columnSQL

-- | 'ColumnSQL' from 'StringSQL'
columnSQL' :: StringSQL -> ColumnSQL
columnSQL' = Internal.columnSQL'

-- | StringSQL from ColumnSQL
showsColumnSQL :: ColumnSQL -> StringSQL
showsColumnSQL = Internal.showsColumnSQL


-- | Compose duplication attribute string.
showsDuplication :: Duplication -> StringSQL
showsDuplication =  dup  where
  dup All      = ALL
  dup Distinct = DISTINCT


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

-- | Empty aggregation.
aggregateEmpty :: [AggregateElem]
aggregateEmpty =  []

showsAggregateColumnRef :: AggregateColumnRef -> StringSQL
showsAggregateColumnRef =  showsColumnSQL

commaed :: [StringSQL] -> StringSQL
commaed =  SQL.fold (|*|)

pComma :: (a -> StringSQL) -> [a] -> StringSQL
pComma qshow =  SQL.paren . commaed . map qshow

showsAggregateBitKey :: AggregateBitKey -> StringSQL
showsAggregateBitKey (AggregateBitKey ts) = pComma showsAggregateColumnRef ts

-- | Compose GROUP BY clause from AggregateElem list.
composeGroupBy :: [AggregateElem] -> StringSQL
composeGroupBy =  d where
  d []       = mempty
  d es@(_:_) = GROUP <> BY <> rec es
  keyList op ss = op <> pComma showsAggregateBitKey ss
  rec = commaed . map showsE
  showsGs (AggregateSet s) = SQL.paren $ rec s
  showsE (ColumnRef t)     = showsAggregateColumnRef t
  showsE (Rollup ss)       = keyList ROLLUP ss
  showsE (Cube   ss)       = keyList CUBE   ss
  showsE (GroupingSets ss) = GROUPING <> SETS <> pComma showsGs ss

-- | Compose PARTITION BY clause from AggregateColumnRef list.
composePartitionBy :: [AggregateColumnRef] -> StringSQL
composePartitionBy =  d where
  d []       = mempty
  d ts@(_:_) = PARTITION <> BY <> commaed (map showsAggregateColumnRef ts)

-- | Extract typed projection from 'AggregateKey'.
aggregateKeyProjection :: AggregateKey a -> a
aggregateKeyProjection (AggregateKey (p, _c)) = p

-- | Extract untyped term from 'AggregateKey'.
aggregateKeyElement :: AggregateKey a -> AggregateElem
aggregateKeyElement (AggregateKey (_p, c)) = c

-- | Unsafely bind typed-projection and untyped-term into 'AggregateKey'.
unsafeAggregateKey :: (a, AggregateElem) -> AggregateKey a
unsafeAggregateKey = AggregateKey


{-# DEPRECATED OrderingTerms "use [OrderingTerm]." #-}
-- | Type for order-by terms
type OrderingTerms = [OrderingTerm]

-- | Compose ORDER BY clause from OrderingTerms
composeOrderBy :: [OrderingTerm] -> StringSQL
composeOrderBy =  d where
  d []       = mempty
  d ts@(_:_) = ORDER <> BY <> commaed (map showsOt ts)
  showsOt (o, e) = showsColumnSQL e <> order o
  order Asc  = ASC
  order Desc = DESC


{-# DEPRECATED Assignments "use [Assignment]." #-}
-- | Assignment pair list.
type Assignments = [Assignment]

-- | Compose SET clause from ['Assignment'].
composeSets :: [Assignment] -> StringSQL
composeSets as = assigns  where
  assignList = foldr (\ (col, term) r ->
                       (showsColumnSQL col .=. showsColumnSQL term) : r)
               [] as
  assigns | null assignList = error "Update assignment list is null!"
          | otherwise       = SET <> commaed assignList

-- | Compose VALUES clause from ['Assignment'].
composeValues :: [Assignment] -> StringSQL
composeValues as = rowConsStringSQL [ showsColumnSQL c | c <- cs ] <> VALUES <>
                   rowConsStringSQL [ showsColumnSQL c | c <- vs ]  where
  (cs, vs) = unzip as


-- | Compose /OVER (PARTITION BY ... )/ clause.
composeOver :: [AggregateColumnRef] -> OrderingTerms -> StringSQL
composeOver pts ots =
  OVER <> SQL.paren (composePartitionBy pts <> composeOrderBy ots)

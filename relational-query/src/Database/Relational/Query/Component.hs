{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Database.Relational.Query.Component
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides untyped components for query.
module Database.Relational.Query.Component (
  -- * Type for column SQL string
  ColumnSQL, columnSQL, columnSQL', showsColumnSQL,

  -- * Configuration type for query
  Config, defaultConfig,
  UnitProductSupport (..), Duplication (..),

  -- * Duplication attribute
  showsDuplication,

  -- * Query restriction
  QueryRestriction, composeWhere, composeHaving,

  -- * Types for aggregation
  AggregateColumnRef,

  AggregateBitKey, AggregateSet, AggregateElem,

  aggregateColumnRef, aggregateEmpty,
  aggregatePowerKey, aggregateGroupingSet,
  aggregateRollup, aggregateCube, aggregateSets,

  composeGroupBy, composePartitionBy,

  -- * Types for ordering
  Order (..), OrderColumn, OrderingTerm, OrderingTerms,
  composeOrderBy,

  -- * Types for assignments
  AssignColumn, AssignTerm, Assignment, Assignments, composeSets,

  -- * Compose window clause
  composeOver
) where

import Data.Monoid (mempty, (<>))

import qualified Database.Relational.Query.Context as Context
import Database.Relational.Query.Expr (Expr, exprAnd)
import Database.Relational.Query.Expr.Unsafe (sqlExpr)

import Database.Relational.Query.Internal.SQL (StringSQL, stringSQL, showStringSQL)
import Language.SQL.Keyword (Keyword(..), (|*|), (.=.))

import qualified Language.SQL.Keyword as SQL


-- | Simple wrap type
newtype ColumnSQL' a = ColumnSQL a

instance Functor ColumnSQL' where
  fmap f (ColumnSQL c) = ColumnSQL $ f c

-- | Column SQL string type
type ColumnSQL = ColumnSQL' StringSQL

-- | 'ColumnSQL' from string
columnSQL :: String -> ColumnSQL
columnSQL =  columnSQL' . stringSQL

-- | 'ColumnSQL' from 'StringSQL'
columnSQL' :: StringSQL -> ColumnSQL
columnSQL' =  ColumnSQL

-- | String from ColumnSQL
stringFromColumnSQL :: ColumnSQL -> String
stringFromColumnSQL =  showStringSQL . showsColumnSQL

-- | StringSQL from ColumnSQL
showsColumnSQL :: ColumnSQL -> StringSQL
showsColumnSQL (ColumnSQL c) = c

instance Show ColumnSQL where
  show = stringFromColumnSQL


-- | Configuration type.
type Config = UnitProductSupport

-- | Default configuration.
defaultConfig :: Config
defaultConfig =  UPSupported

-- | Unit product is supported or not.
data UnitProductSupport = UPSupported | UPNotSupported  deriving Show


-- | Result record duplication attribute
data Duplication = All | Distinct  deriving Show

-- | Compose duplication attribute string.
showsDuplication :: Duplication -> StringSQL
showsDuplication =  dup  where
  dup All      = ALL
  dup Distinct = DISTINCT


-- | Type for restriction of query.
type QueryRestriction c = [Expr c Bool]

-- | Compose SQL String from 'QueryRestriction'.
composeRestrict :: Keyword -> QueryRestriction c -> StringSQL
composeRestrict k = d  where
  d    []    =  mempty
  d e@(_:_)  =  k <> sqlExpr (foldr1 exprAnd e)

-- | Compose WHERE clause from 'QueryRestriction'.
composeWhere :: QueryRestriction Context.Flat -> StringSQL
composeWhere =  composeRestrict WHERE

-- | Compose HAVING clause from 'QueryRestriction'.
composeHaving :: QueryRestriction Context.Aggregated -> StringSQL
composeHaving =  composeRestrict HAVING


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

-- | Order direction. Ascendant or Descendant.
data Order = Asc | Desc  deriving Show

-- | Type for order-by column
type OrderColumn = ColumnSQL

-- | Type for order-by term
type OrderingTerm = (Order, OrderColumn)

-- | Type for order-by terms
type OrderingTerms = [OrderingTerm]

-- | Compose ORDER BY clause from OrderingTerms
composeOrderBy :: OrderingTerms -> StringSQL
composeOrderBy =  d where
  d []       = mempty
  d ts@(_:_) = ORDER <> BY <> commaed (map showsOt ts)
  showsOt (o, e) = showsColumnSQL e <> order o
  order Asc  = ASC
  order Desc = DESC


-- | Column SQL String
type AssignColumn = ColumnSQL

-- | Value SQL String
type AssignTerm   = ColumnSQL

-- | Assignment pair
type Assignment = (AssignColumn, AssignTerm)

-- | Assignment pair list.
type Assignments = [Assignment]

-- | Compose SET clause from 'Assignments'.
composeSets :: Assignments -> StringSQL
composeSets as = assigns  where
  assignList = foldr (\ (col, term) r ->
                       (showsColumnSQL col .=. showsColumnSQL term) : r)
               [] as
  assigns | null assignList = error "Update assignment list is null!"
          | otherwise       = SET <> commaed assignList


-- | Compose /OVER (PARTITION BY ... )/ clause.
composeOver :: [AggregateColumnRef] -> OrderingTerms -> StringSQL
composeOver pts ots =
  OVER <> SQL.paren (composePartitionBy pts <> composeOrderBy ots)

{-# LANGUAGE OverloadedStrings #-}

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
  ColumnSQL, columnSQL, sqlWordFromColumn, stringFromColumnSQL,

  -- * Configuration type for query
  Config, defaultConfig,
  UnitProductSupport (..),

  -- * Query restriction
  QueryRestriction, composeWhere, composeHaving,

  -- * Types for aggregation
  AggregateTerm, AggregateTerms,

  AggregateKey, AggregateElem,

  aggregateTerm, aggregateEmpty,
  aggregatePowerKey, aggregateRollup, aggregateCube, aggregateSets,


  composeGroupBy,

  -- * Types for ordering
  Order (..), OrderColumn, OrderingTerm, OrderingTerms,

  -- * Types for assignments
  AssignColumn, AssignTerm, Assignment, Assignments, composeSets
  ) where

import qualified Database.Relational.Query.Context as Context
import Database.Relational.Query.Expr (Expr)
import Database.Relational.Query.Expr.Unsafe (showExpr)

import Database.Relational.Query.Internal.String
  (showUnwordsSQL, showWordSQL, showSpace, showParen', showSepBy)
import Language.SQL.Keyword (Keyword(..))

import qualified Language.SQL.Keyword as SQL

-- | Column SQL string type
newtype ColumnSQL = ColumnSQL String

-- | 'ColumnSQL' from string
columnSQL :: String -> ColumnSQL
columnSQL =  ColumnSQL

-- | String from ColumnSQL
stringFromColumnSQL :: ColumnSQL -> String
stringFromColumnSQL (ColumnSQL s) = s

-- | SQL word from 'ColumnSQL'
sqlWordFromColumn :: ColumnSQL -> SQL.Keyword
sqlWordFromColumn =  SQL.word . stringFromColumnSQL

instance Show ColumnSQL where
  show = stringFromColumnSQL


-- | Configuration type.
type Config = UnitProductSupport

-- | Default configuration.
defaultConfig :: Config
defaultConfig =  UPSupported

-- | Unit product is supported or not.
data UnitProductSupport = UPSupported | UPNotSupported  deriving Show


-- | Type for restriction of query.
type QueryRestriction c = Maybe (Expr c Bool)

-- | Compose SQL String from 'QueryRestriction'.
composeRestrict :: Keyword -> QueryRestriction c -> ShowS
composeRestrict k = maybe id (\e -> showSpace . showUnwordsSQL [k, SQL.word . showExpr $ e])

-- | Compose WHERE clause from 'QueryRestriction'.
composeWhere :: QueryRestriction Context.Flat -> ShowS
composeWhere =  composeRestrict WHERE

-- | Compose HAVING clause from 'QueryRestriction'.
composeHaving :: QueryRestriction Context.Aggregated -> ShowS
composeHaving =  composeRestrict HAVING


-- | Type for group-by term
type AggregateTerm = ColumnSQL

-- | Type for group-by terms
type AggregateTerms = [AggregateTerm]

-- | Type for group key.
newtype AggregateKey = AggregateKey [AggregateTerm] deriving Show

-- | Type for group-by tree
data AggregateElem = Term AggregateTerm
                   | Rollup [AggregateKey]
                   | Cube   [AggregateKey]
                   | GroupingSets [[AggregateElem]]
                   deriving Show

-- | Single term aggregation element.
aggregateTerm :: AggregateTerm -> AggregateElem
aggregateTerm =  Term

-- | Key of aggregation power set.
aggregatePowerKey :: [AggregateTerm] -> AggregateKey
aggregatePowerKey =  AggregateKey

-- | Rollup aggregation element.
aggregateRollup :: [AggregateKey] -> AggregateElem
aggregateRollup =  Rollup

-- | Cube aggregation element.
aggregateCube :: [AggregateKey] -> AggregateElem
aggregateCube =  Cube

-- | Grouping sets aggregation.
aggregateSets :: [[AggregateElem]] -> AggregateElem
aggregateSets =  GroupingSets

-- | Empty aggregation.
aggregateEmpty :: [AggregateElem]
aggregateEmpty =  []

comma :: ShowS
comma =  showString ", "

showsAggregateTerm :: AggregateTerm -> ShowS
showsAggregateTerm =  showString . stringFromColumnSQL

parenSepByComma :: (a -> ShowS) -> [a] -> ShowS
parenSepByComma shows' = showParen' . (`showSepBy` comma) . map shows'

showsAggregateKey :: AggregateKey -> ShowS
showsAggregateKey (AggregateKey ts) = parenSepByComma showsAggregateTerm ts

-- | Compose GROUP BY clause from AggregateElem list.
composeGroupBy :: [AggregateElem] -> ShowS
composeGroupBy es = showSpace . showUnwordsSQL [GROUP, BY] . showSpace . rec es  where
  keyList op ss = showWordSQL op . parenSepByComma showsAggregateKey ss
  rec = (`showSepBy` comma) . map d
  d (Term t)          = showsAggregateTerm t
  d (Rollup ss)       = keyList ROLLUP ss
  d (Cube   ss)       = keyList CUBE   ss
  d (GroupingSets ss) = showUnwordsSQL [GROUPING, SETS] . showSpace
                        . parenSepByComma (showParen' . rec) ss

-- | Order direction. Ascendant or Descendant.
data Order = Asc | Desc  deriving Show

-- | Type for order-by column
type OrderColumn = ColumnSQL

-- | Type for order-by term
type OrderingTerm = (Order, OrderColumn)

-- | Type for order-by terms
type OrderingTerms = [OrderingTerm]


-- | Column SQL String
type AssignColumn = ColumnSQL

-- | Value SQL String
type AssignTerm   = ColumnSQL

-- | Assignment pair
type Assignment = (AssignColumn, AssignTerm)

-- | Assignment pair list.
type Assignments = [Assignment]

-- | Compose SET clause from 'Assignments'.
composeSets :: Assignments -> ShowS
composeSets as = assigns  where
  assignList = foldr (\ (col, term) r ->
                       [sqlWordFromColumn col, sqlWordFromColumn term] `SQL.sepBy` " = "  : r)
               [] as
  assigns | null assignList = error "Update assignment list is null!"
          | otherwise       = showSpace . showUnwordsSQL [SET, assignList `SQL.sepBy` ", "]

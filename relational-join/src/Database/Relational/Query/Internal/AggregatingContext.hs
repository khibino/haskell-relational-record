{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Database.Relational.Query.Internal.AggregatingContext
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines context of aggregated query.
module Database.Relational.Query.Internal.AggregatingContext (
  -- * Context of aggregated query
  AggregatingContext,

  primeAggregatingContext,

  addGroupBy, addRestriction,

  composeGroupBys
  ) where

import Data.DList (DList)
import qualified Data.DList as DList
import Data.Monoid ((<>))
import Control.Applicative (pure)

import Database.Relational.Query.Expr (Expr, showExpr, fromTriBool, exprAnd)
import Database.Relational.Query.Aggregation (Aggregation)

import Language.SQL.Keyword (Keyword(..), unwordsSQL)
import qualified Language.SQL.Keyword as SQL
import qualified Language.SQL.Keyword.ConcatString as SQLs


-- | Internal type for group-by term
type GroupByTerm = String

-- | Group-by terms type
type GroupBys = DList GroupByTerm

-- | Context state of aggregated query.
data AggregatingContext =
  AggregatingContext
  { groupByTerms :: GroupBys
  , restriction  :: Maybe (Expr Aggregation Bool)
  }

-- | Initial value of 'AggregatingContext'.
primeAggregatingContext :: AggregatingContext
primeAggregatingContext =  AggregatingContext DList.empty Nothing

-- | Add group by term into 'AggregatingContext'.
addGroupBy :: String -> AggregatingContext -> AggregatingContext
addGroupBy t c =  c { groupByTerms = groupByTerms c <> pure t }

-- | Add having restriction into 'AggregatingContext'.
addRestriction :: Expr Aggregation (Maybe Bool) -> AggregatingContext -> AggregatingContext
addRestriction e1 ctx =
  ctx { restriction = Just . uf . restriction $ ctx }
  where uf  Nothing  = fromTriBool e1
        uf (Just e0) = e0 `exprAnd` fromTriBool e1

-- | Extract 'AggregatingContext' into SQL string.
composeGroupBys :: AggregatingContext -> String
composeGroupBys ac = unwordsSQL $ groupBys ++ havings
  where groupBys
          | null gs   = []
          | otherwise = [GROUP, BY, SQL.word . concat $ gs `SQLs.sepBy` ", "]
        gs = DList.toList (groupByTerms ac)
        havings = maybe [] (\e -> [HAVING, SQL.word . showExpr $ e]) $ restriction ac

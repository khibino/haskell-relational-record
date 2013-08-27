{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Database.Relational.Query.Monad.Trans.AggregatingState
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides state definition for
-- "Database.Relational.Query.Monad.Trans.Aggregating".
module Database.Relational.Query.Monad.Trans.AggregatingState (
  -- * Context of aggregated query
  AggregatingContext,

  primeAggregatingContext,

  addGroupBy,

  aggregateTerms,

  composeGroupBys
  ) where

import Data.DList (DList)
import qualified Data.DList as DList
import Data.Monoid ((<>))
import Control.Applicative (pure)

import Database.Relational.Query.Table (sqlWordFromColumn)
import Database.Relational.Query.Sub (AggregateTerm, AggregateTerms)

import Language.SQL.Keyword (Keyword(..), unwordsSQL)
import qualified Language.SQL.Keyword as SQL


-- | Context state of aggregated query.
newtype AggregatingContext =
  AggregatingContext
  { groupByTerms :: DList AggregateTerm }

-- | Initial value of 'AggregatingContext'.
primeAggregatingContext :: AggregatingContext
primeAggregatingContext =  AggregatingContext DList.empty

-- | Add group by term into 'AggregatingContext'.
addGroupBy :: AggregateTerm -> AggregatingContext -> AggregatingContext
addGroupBy t c =  c { groupByTerms = groupByTerms c <> pure t }

-- | Finalize context to extract accumulated aggregate terms state.
aggregateTerms :: AggregatingContext -> AggregateTerms
aggregateTerms =  DList.toList . groupByTerms

-- | Extract 'AggregatingContext' into SQL string.
composeGroupBys :: AggregatingContext -> String
composeGroupBys ac = unwordsSQL $ groupBys
  where groupBys
          | null gs   = []
          | otherwise = [GROUP, BY, gs `SQL.sepBy` ", "]
        gs = map sqlWordFromColumn $ DList.toList (groupByTerms ac)

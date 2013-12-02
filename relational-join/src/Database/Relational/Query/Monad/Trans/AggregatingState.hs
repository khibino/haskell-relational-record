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

  aggregateTerms
  ) where

import Data.DList (DList)
import qualified Data.DList as DList
import Data.Monoid ((<>))
import Control.Applicative (pure)

import Database.Relational.Query.Component (AggregateColumnRef, AggregateTerms)


-- | Context state of aggregated query.
newtype AggregatingContext =
  AggregatingContext
  { groupByTerms :: DList AggregateColumnRef }

-- | Initial value of 'AggregatingContext'.
primeAggregatingContext :: AggregatingContext
primeAggregatingContext =  AggregatingContext DList.empty

-- | Add group by term into 'AggregatingContext'.
addGroupBy :: AggregateColumnRef -> AggregatingContext -> AggregatingContext
addGroupBy t c =  c { groupByTerms = groupByTerms c <> pure t }

-- | Finalize context to extract accumulated aggregate terms state.
aggregateTerms :: AggregatingContext -> AggregateTerms
aggregateTerms =  DList.toList . groupByTerms

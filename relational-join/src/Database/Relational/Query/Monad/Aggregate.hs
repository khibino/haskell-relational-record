{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Query.Monad.Aggregate
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains definitions about aggregated query type.
module Database.Relational.Query.Monad.Aggregate (
  -- * Aggregated Query
  QueryAggregate,
  AggregatedQuery,

  toSQL,

  toSubQuery,

  AggregatePower, rollup, cube,
  AggregateSetList, groupingSets
  ) where

import Control.Arrow (second)
import Data.Functor.Identity (Identity (..), runIdentity)

import Database.Relational.Query.Context (Flat, Aggregated)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Component
  (QueryRestriction, OrderingTerms, AggregateElem, AggregateKey,
   aggregateRollup, aggregateCube, aggregateSets)
import Database.Relational.Query.Sub (SubQuery, aggregatedSubQuery, JoinProduct)
import qualified Database.Relational.Query.Sub as SubQuery

import Database.Relational.Query.Monad.Class (MonadRestrict(..), MonadQualify(..))
import Database.Relational.Query.Monad.Trans.Config (askConfig)
import Database.Relational.Query.Monad.Trans.Join (join')
import Database.Relational.Query.Monad.Trans.Restricting
  (Restrictings, restrictings, extractRestrict)
import Database.Relational.Query.Monad.Trans.Aggregating
  (aggregatings, extractAggregateTerms, Aggregatings,
   AggregatingSet, AggregatingPowerSet, AggregatingSetList)
import Database.Relational.Query.Monad.Trans.Ordering
  (Orderings, orderings, OrderedQuery, extractOrderingTerms)
import Database.Relational.Query.Monad.Type (ConfigureQuery, QueryCore, extractCore)


-- | Aggregated query monad type.
type QueryAggregate     = Orderings Aggregated (Restrictings Aggregated (AggregatingSet QueryCore))

-- | Aggregated query type. AggregatedQuery r == QueryAggregate (Projection Aggregated r).
type AggregatedQuery  r = OrderedQuery Aggregated (Restrictings Aggregated (AggregatingSet QueryCore)) r

-- | Lift from qualified table forms into 'QueryAggregate'.
aggregatedQuery :: ConfigureQuery a -> QueryAggregate a
aggregatedQuery =  orderings . restrictings . aggregatings . restrictings . join'

-- | Restricted 'MonadRestrict' instance.
instance MonadRestrict Flat q => MonadRestrict Flat (Restrictings Aggregated q) where
  restrictContext = restrictings . restrictContext

-- | Instance to lift from qualified table forms into 'QueryAggregate'.
instance MonadQualify ConfigureQuery QueryAggregate where
  liftQualify = aggregatedQuery

extract :: AggregatedQuery r
        -> ConfigureQuery (((((Projection Aggregated r, OrderingTerms),
                                 QueryRestriction Aggregated),
                                [AggregateElem]),
                               QueryRestriction Flat),
                              JoinProduct)
extract =  extractCore . extractAggregateTerms . extractRestrict . extractOrderingTerms

-- | Run 'AggregatedQuery' to get SQL with 'ConfigureQuery' computation.
toSQL :: AggregatedQuery r     -- ^ 'AggregatedQuery' to run
      -> ConfigureQuery String -- ^ Result SQL string with 'ConfigureQuery' computation
toSQL =  fmap SubQuery.toSQL . toSubQuery

-- | Run 'AggregatedQuery' to get 'SubQuery' with 'ConfigureQuery' computation.
toSubQuery :: AggregatedQuery r       -- ^ 'AggregatedQuery' to run
           -> ConfigureQuery SubQuery -- ^ Result 'SubQuery' with 'ConfigureQuery' computation
toSubQuery q = do
  (((((pj, ot), grs), ag), rs), pd) <- extract q
  c <- askConfig
  return $ aggregatedSubQuery c (Projection.untype pj) pd rs ag grs ot


extractTermList :: Aggregatings ac at Identity a -> (a, [at])
extractTermList =  runIdentity . extractAggregateTerms

-- | Context monad type to build grouping power set.
type AggregatePower = AggregatingPowerSet Identity

finalizePower :: ([AggregateKey] -> AggregateElem)
              -> AggregatePower a -> (a, AggregateElem)
finalizePower finalize pow = second finalize . extractTermList $ pow

-- | Finalize grouping power set as rollup power set.
rollup :: AggregatePower a -> (a, AggregateElem)
rollup =  finalizePower aggregateRollup

-- | Finalize grouping power set as cube power set.
cube   :: AggregatePower a -> (a, AggregateElem)
cube   =  finalizePower aggregateCube

-- | Context monad type to build grouping set list.
type AggregateSetList = AggregatingSetList Identity

-- | Finalize grouping set list.
groupingSets :: Monad m => AggregateSetList a -> (a, AggregateElem)
groupingSets =  second aggregateSets . extractTermList

{-# OPTIONS_GHC -fno-warn-orphans #-}
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

  toSubQuery
  ) where

import Database.Relational.Query.Context (Flat, Aggregated)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Sub
  (SubQuery, aggregatedSubQuery, JoinProduct, QueryRestriction, AggregateTerms, OrderingTerms)
import qualified Database.Relational.Query.Sub as SubQuery

import Database.Relational.Query.Monad.Class (MonadQualify(..))
import Database.Relational.Query.Monad.Trans.Config (askConfig)
import Database.Relational.Query.Monad.Trans.Join (join')
import Database.Relational.Query.Monad.Trans.Restricting
  (Restrictings, restrictings, extractRestrict)
import Database.Relational.Query.Monad.Trans.Aggregating
  (Aggregatings, aggregatings, extractAggregateTerms)
import Database.Relational.Query.Monad.Trans.Ordering
  (Orderings, orderings, OrderedQuery, extractOrderingTerms)
import Database.Relational.Query.Monad.Type (ConfigureQuery, QueryCore, extractCore)


-- | Aggregated query monad type.
type QueryAggregate    = Orderings Aggregated (Restrictings Aggregated (Aggregatings QueryCore))

-- | Aggregated query type. AggregatedQuery r == QueryAggregate (Projection Aggregated r).
type AggregatedQuery r = OrderedQuery Aggregated (Restrictings Aggregated (Aggregatings QueryCore)) r

-- | Lift from qualified table forms into 'QueryAggregate'.
aggregatedQuery :: ConfigureQuery a -> QueryAggregate a
aggregatedQuery =  orderings . restrictings . aggregatings . restrictings . join'

-- | Instance to lift from qualified table forms into 'QueryAggregate'.
instance MonadQualify ConfigureQuery QueryAggregate where
  liftQualify = aggregatedQuery

extract :: AggregatedQuery r
        -> ConfigureQuery (((((Projection Aggregated r, OrderingTerms),
                                 QueryRestriction Aggregated),
                                AggregateTerms),
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

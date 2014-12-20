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
  AggregatedQuery, AggregatedQuery',

  toSQL,

  toSubQuery,

  Window, partitionBy, over
  ) where

import Data.Functor.Identity (Identity (runIdentity))

import Database.Relational.Query.Internal.SQL (showStringSQL)
import Database.Relational.Query.Context (Flat, Aggregated, OverWindow)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Component
  (AggregateColumnRef, Duplication, QueryRestriction, OrderingTerms, AggregateElem, composeOver)
import Database.Relational.Query.Sub (SubQuery, aggregatedSubQuery, JoinProduct)
import qualified Database.Relational.Query.Sub as SubQuery
import Database.Relational.Query.Projectable (PlaceHolders, SqlProjectable, unsafeProjectSql, unsafeShowSql)

import Database.Relational.Query.Monad.Class (MonadRestrict(..), MonadQualify(..), MonadPartition (..))
import Database.Relational.Query.Monad.Trans.Join (join')
import Database.Relational.Query.Monad.Trans.Restricting
  (Restrictings, restrictings, extractRestrict)
import Database.Relational.Query.Monad.Trans.Aggregating
  (aggregatings, extractAggregateTerms, AggregatingSetT, PartitioningSet)
import Database.Relational.Query.Monad.Trans.Ordering
  (Orderings, orderings, extractOrderingTerms)
import Database.Relational.Query.Monad.Type
  (ConfigureQuery, askConfig, QueryCore, extractCore, OrderedQuery, OrderedQuery')


-- | Aggregated query monad type.
type QueryAggregate     = Orderings Aggregated (Restrictings Aggregated (AggregatingSetT QueryCore))

-- | Aggregated query type. 'AggregatedQuery' r == 'QueryAggregate' ('Projection' 'Aggregated' r).
type AggregatedQuery  r = OrderedQuery Aggregated (Restrictings Aggregated (AggregatingSetT QueryCore)) r

-- | Aggregated query type. 'AggregatedQuery'' p r == 'QueryAggregate' ('PlaceHolders' p, 'Projection' 'Aggregated' r).
type AggregatedQuery' p r = OrderedQuery' Aggregated (Restrictings Aggregated (AggregatingSetT QueryCore)) p r

-- | Partition monad type for partition-by clause.
type Window           c = Orderings c (PartitioningSet c)

-- | Lift from qualified table forms into 'QueryAggregate'.
aggregatedQuery :: ConfigureQuery a -> QueryAggregate a
aggregatedQuery =  orderings . restrictings . aggregatings . restrictings . join'

-- | Restricted 'MonadRestrict' instance.
instance MonadRestrict Flat q => MonadRestrict Flat (Restrictings Aggregated q) where
  restrictContext = restrictings . restrictContext

-- | Instance to lift from qualified table forms into 'QueryAggregate'.
instance MonadQualify ConfigureQuery QueryAggregate where
  liftQualify = aggregatedQuery

extract :: AggregatedQuery' p r
        -> ConfigureQuery (((((((PlaceHolders p, Projection Aggregated r), OrderingTerms),
                               QueryRestriction Aggregated),
                              [AggregateElem]),
                             QueryRestriction Flat),
                            JoinProduct), Duplication)
extract =  extractCore . extractAggregateTerms . extractRestrict . extractOrderingTerms

-- | Run 'AggregatedQuery' to get SQL with 'ConfigureQuery' computation.
toSQL :: AggregatedQuery' p r     -- ^ 'AggregatedQuery' to run
      -> ConfigureQuery String -- ^ Result SQL string with 'ConfigureQuery' computation
toSQL =  fmap SubQuery.toSQL . toSubQuery

-- | Run 'AggregatedQuery' to get 'SubQuery' with 'ConfigureQuery' computation.
toSubQuery :: AggregatedQuery' p r       -- ^ 'AggregatedQuery' to run
           -> ConfigureQuery SubQuery -- ^ Result 'SubQuery' with 'ConfigureQuery' computation
toSubQuery q = do
  (((((((_ph, pj), ot), grs), ag), rs), pd), da) <- extract q
  c <- askConfig
  return $ aggregatedSubQuery c (Projection.untype pj) da pd rs ag grs ot

-- | Add /PARTITION BY/ term into context.
partitionBy :: Projection c r -> Window c ()
partitionBy =  mapM_ unsafeAddPartitionKey . Projection.columns

extractWindow :: Window c a -> ((a, OrderingTerms), [AggregateColumnRef])
extractWindow =  runIdentity . extractAggregateTerms . extractOrderingTerms

-- | Operator to make window function projection.
over :: SqlProjectable (Projection c)
     => Projection OverWindow a
     -> Window c ()
     -> Projection c a
wp `over` win = unsafeProjectSql $ unwords [unsafeShowSql wp, showStringSQL (composeOver pt ot)]  where
  (((), ot), pt) = extractWindow win

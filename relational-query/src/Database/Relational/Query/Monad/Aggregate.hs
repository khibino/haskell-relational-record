{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Query.Monad.Aggregate
-- Copyright   : 2013-2017 Kei Hibino
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

  Window, over
  ) where

import Data.Functor.Identity (Identity (runIdentity))
import Data.Monoid ((<>))

import Language.SQL.Keyword (Keyword(..))
import qualified Language.SQL.Keyword as SQL

import Database.Relational.Query.Internal.BaseSQL (Duplication, OrderingTerm, composeOrderBy)
import Database.Relational.Query.Internal.GroupingSQL (AggregateColumnRef, AggregateElem, composePartitionBy)

import Database.Relational.Query.Context (Flat, Aggregated, OverWindow)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Sub (SubQuery, QueryRestriction, JoinProduct, aggregatedSubQuery)
import qualified Database.Relational.Query.Sub as SubQuery
import Database.Relational.Query.Projectable (PlaceHolders, SqlProjectable)
import Database.Relational.Query.Monad.Class (MonadRestrict(..))
import Database.Relational.Query.Monad.Trans.Restricting
  (Restrictings, restrictings, extractRestrict)
import Database.Relational.Query.Monad.Trans.Aggregating
  (extractAggregateTerms, AggregatingSetT, PartitioningSet)
import Database.Relational.Query.Monad.Trans.Ordering
  (Orderings, extractOrderingTerms)
import Database.Relational.Query.Monad.BaseType (ConfigureQuery, askConfig)
import Database.Relational.Query.Monad.Type (QueryCore, extractCore, OrderedQuery)


-- | Aggregated query monad type.
type QueryAggregate     = Orderings Aggregated (Restrictings Aggregated (AggregatingSetT QueryCore))

-- | Aggregated query type. 'AggregatedQuery' p r == 'QueryAggregate' ('PlaceHolders' p, 'Projection' 'Aggregated' r).
type AggregatedQuery p r = OrderedQuery Aggregated (Restrictings Aggregated (AggregatingSetT QueryCore)) p r

-- | Partition monad type for partition-by clause.
type Window           c = Orderings c (PartitioningSet c)

-- | Restricted 'MonadRestrict' instance.
instance MonadRestrict Flat q => MonadRestrict Flat (Restrictings Aggregated q) where
  restrict = restrictings . restrict

extract :: AggregatedQuery p r
        -> ConfigureQuery (((((((PlaceHolders p, Projection Aggregated r), [OrderingTerm]),
                               QueryRestriction Aggregated),
                              [AggregateElem]),
                             QueryRestriction Flat),
                            JoinProduct), Duplication)
extract =  extractCore . extractAggregateTerms . extractRestrict . extractOrderingTerms

-- | Run 'AggregatedQuery' to get SQL with 'ConfigureQuery' computation.
toSQL :: AggregatedQuery p r     -- ^ 'AggregatedQuery' to run
      -> ConfigureQuery String -- ^ Result SQL string with 'ConfigureQuery' computation
toSQL =  fmap SubQuery.toSQL . toSubQuery

-- | Run 'AggregatedQuery' to get 'SubQuery' with 'ConfigureQuery' computation.
toSubQuery :: AggregatedQuery p r       -- ^ 'AggregatedQuery' to run
           -> ConfigureQuery SubQuery -- ^ Result 'SubQuery' with 'ConfigureQuery' computation
toSubQuery q = do
  (((((((_ph, pj), ot), grs), ag), rs), pd), da) <- extract q
  c <- askConfig
  return $ aggregatedSubQuery c (Projection.untype pj) da pd rs ag grs ot

extractWindow :: Window c a -> ((a, [OrderingTerm]), [AggregateColumnRef])
extractWindow =  runIdentity . extractAggregateTerms . extractOrderingTerms

-- | Operator to make window function result projection using built 'Window' monad.
over :: SqlProjectable (Projection c)
     => Projection OverWindow a
     -> Window c ()
     -> Projection c a
wp `over` win =
  Projection.unsafeFromSqlTerms
  [ c <> OVER <> SQL.paren (composePartitionBy pt <> composeOrderBy ot)
  | c <- Projection.columns wp
  ]  where (((), ot), pt) = extractWindow win

infix 8 `over`

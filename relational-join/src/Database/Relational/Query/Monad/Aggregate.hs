{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
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
-- This module defines definitions about aggregated query type.
module Database.Relational.Query.Monad.Aggregate (
  -- * Aggregated Query
  QueryAggregate,
  AggregatedQuery,

  toSQL,

  toSubQuery
  ) where

import Control.Applicative ((<$>))

import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Aggregation (Aggregation)
import qualified Database.Relational.Query.Aggregation as Aggregation
import Database.Relational.Query.Sub (SubQuery, subQuery)

import Database.Relational.Query.Monad.Qualify (Qualify)
import Database.Relational.Query.Monad.Class (MonadQualify(..))
import Database.Relational.Query.Monad.Trans.Join (join')
import qualified Database.Relational.Query.Monad.Trans.Join as Join
import Database.Relational.Query.Monad.Trans.Ordering (Orderings, orderings, OrderedQuery, OrderByAppend, orderByAppend)
import qualified Database.Relational.Query.Monad.Trans.Ordering as Ordering
import Database.Relational.Query.Monad.Trans.Restrict (restrict, WheresAppend, wheresAppend)
import qualified Database.Relational.Query.Monad.Trans.Restrict as Restrict
import Database.Relational.Query.Monad.Core (QueryCore)
import Database.Relational.Query.Monad.Trans.Aggregate (Aggregatings, aggregate, appendGroupBys)


-- | Aggregated query monad type.
type QueryAggregate    = Orderings Aggregation (Aggregatings QueryCore)

-- | Aggregated query type. AggregatedQuery r == QueryAggregate (Aggregation r).
type AggregatedQuery r = OrderedQuery Aggregation (Aggregatings QueryCore) r

-- | Lift from qualified table forms into 'QueryAggregate'.
aggregatedQuery :: Qualify a -> QueryAggregate a
aggregatedQuery =  orderings . aggregate . restrict . join'

-- | Instance to lift from qualified table forms into 'QueryAggregate'.
instance MonadQualify Qualify (Orderings Aggregation (Aggregatings QueryCore)) where
  liftQualify = aggregatedQuery

-- | Run 'AggregatedQuery' to get SQL string.
expandSQL :: AggregatedQuery r -> Qualify ((String, Projection r), ((OrderByAppend, String -> String), WheresAppend))
expandSQL q = Join.expandSQL $ assoc <$> Restrict.appendWheres (appendGroupBys (Ordering.appendOrderBys q))  where
  assoc (((a, b), c), d) = (Aggregation.unsafeProjection a, ((b, c), d))

-- | Run 'AggregatedQuery' to get SQL with 'Qualify' computation.
toSQL :: AggregatedQuery r -- ^ 'AggregatedQuery' to run
      -> Qualify String    -- ^ Result SQL string with 'Qualify' computation
toSQL q = do
  ((sql, _pj), ((appOrd, appGrp), appWhere)) <- expandSQL q
  return . orderByAppend appOrd . appGrp . wheresAppend appWhere $ sql

-- | Run 'AggregatedQuery' to get 'SubQuery' with 'Qualify' computation.
toSubQuery :: AggregatedQuery r -- ^ 'AggregatedQuery' to run
           -> Qualify SubQuery  -- ^ Result 'SubQuery' with 'Qualify' computation
toSubQuery q = do
  ((sql, pj), ((appOrd, appGrp), appWhere)) <- expandSQL q
  return $ subQuery (orderByAppend appOrd . appGrp . wheresAppend appWhere $ sql) (Projection.width pj)

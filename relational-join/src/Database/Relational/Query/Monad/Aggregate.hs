{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Relational.Query.Monad.Aggregate (
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
import Database.Relational.Query.Monad.Trans.Join (QueryJoin, join')
import qualified Database.Relational.Query.Monad.Trans.Join as Join
import Database.Relational.Query.Monad.Trans.Ordering (Orderings, orderings, OrderedQuery)
import qualified Database.Relational.Query.Monad.Trans.Ordering as Ordering
import Database.Relational.Query.Monad.Core (QueryCore)
import Database.Relational.Query.Monad.Trans.Aggregate (Aggregatings, aggregate, appendGroupBys)


type QueryAggregate    = Orderings Aggregation (Aggregatings QueryCore)
type AggregatedQuery r = OrderedQuery Aggregation (Aggregatings QueryCore) r

aggregatedQuery :: Qualify a -> QueryAggregate a
aggregatedQuery =  orderings . aggregate . join'

instance MonadQualify Qualify (Orderings Aggregation (Aggregatings (QueryJoin Qualify))) where
  liftQualify = aggregatedQuery

expandSQL :: QueryAggregate (Aggregation r) -> Qualify ((String, Projection r), (String -> String, String -> String))
expandSQL q = Join.expandSQL $ assoc <$> appendGroupBys (Ordering.appendOrderBys q)  where
  assoc ((a, b), c) = (Aggregation.projection a, (b, c))

toSQL :: QueryAggregate (Aggregation r) -> Qualify String
toSQL q = do
  ((sql, _pj), (appOrd, appGrp)) <- expandSQL q
  return . appOrd $ appGrp sql

toSubQuery :: QueryAggregate (Aggregation r) -> Qualify SubQuery
toSubQuery q = do
  ((sql, pj), (appOrd, appGrp)) <- expandSQL q
  return $ subQuery (appOrd $ appGrp sql) (Projection.width pj)

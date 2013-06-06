{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.Relational.Query.Relation
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines re-usable Relation type
-- to compose complex query.
module Database.Relational.Query.Relation (
  -- * Relation type
  Relation,

  table,
  relation, relation',
  aggregateRelation, aggregateRelation',

  nested, width,

  sqlFromRelation,

  -- * Query using relation
  query, query', queryMaybe, queryMaybe', from,

  -- * Direct style join
  JoinRestriction,
  inner', left', right', full',
  inner, left, right, full,
  on',
  ) where

import Database.Relational.Query.Monad.Qualify (Qualify, evalQualifyPrime, qualifyQuery)
import Database.Relational.Query.Monad.Class
  (MonadQualify (liftQualify), MonadQuery (on, unsafeSubQuery))
import Database.Relational.Query.Monad.Simple (QuerySimple, SimpleQuery)
import qualified Database.Relational.Query.Monad.Simple as Simple
import Database.Relational.Query.Monad.Aggregate (QueryAggregate, AggregatedQuery)
import qualified Database.Relational.Query.Monad.Aggregate as Aggregate

import Database.Relational.Query.Table (Table)

import Database.Relational.Query.Internal.Product (NodeAttr(Just', Maybe))

import Database.Relational.Query.Expr (Expr)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Aggregation (Aggregation)
import Database.Relational.Query.Projectable
  (PlaceHolders, addPlaceHolders, projectZip)
import Database.Relational.Query.ProjectableExtended
  (ProjectableGeneralizedZip (generalizedZip))

import Database.Relational.Query.Sub (SubQuery)
import qualified Database.Relational.Query.Sub as SubQuery


data Relation p r = SubQuery SubQuery
                  | SimpleRel (SimpleQuery r)
                  | AggregateRel (AggregatedQuery r)


table :: Table r -> Relation () r
table =  SubQuery . SubQuery.fromTable

from :: Table r -> Relation () r
from =  table


subQueryQualifyFromRelation :: Relation p r -> Qualify SubQuery
subQueryQualifyFromRelation =  d  where
  d (SubQuery sub)    = return $ sub
  d (SimpleRel qp)    = Simple.toSubQuery qp
  d (AggregateRel qp) = Aggregate.toSubQuery qp

subQueryFromRelation :: Relation p r -> SubQuery
subQueryFromRelation =  evalQualifyPrime . subQueryQualifyFromRelation

queryWithAttr :: MonadQualify Qualify m
              => NodeAttr -> Relation p r -> m (PlaceHolders p, Projection r)
queryWithAttr attr = addPlaceHolders . run where
  run rel = do
    q <- liftQualify $ do
      sq <- subQueryQualifyFromRelation rel
      qualifyQuery sq
    unsafeSubQuery attr q
  -- d (Relation q) = unsafeMergeAnotherQuery attr q

query' :: MonadQualify Qualify m => Relation p r -> m (PlaceHolders p, Projection r)
query' =  queryWithAttr Just'

query :: MonadQualify Qualify m => Relation () r -> m (Projection r)
query =  fmap snd . query'

queryMaybe' :: MonadQualify Qualify m => Relation p r -> m (PlaceHolders p, Projection (Maybe r))
queryMaybe' pr =  do
  (ph, pj) <- queryWithAttr Maybe pr
  return (ph, Projection.just pj)

queryMaybe :: MonadQualify Qualify m => Relation () r -> m (Projection (Maybe r))
queryMaybe =  fmap snd . queryMaybe'

relation :: QuerySimple (Projection r) -> Relation () r
relation =  SimpleRel

relation' :: QuerySimple (PlaceHolders p, Projection r) -> Relation p r
relation' =  SimpleRel . fmap snd

aggregateRelation :: QueryAggregate (Aggregation r) -> Relation () r
aggregateRelation =  AggregateRel

aggregateRelation' :: QueryAggregate (PlaceHolders p, Aggregation r) -> Relation p r
aggregateRelation' =  AggregateRel . fmap snd


type JoinRestriction a b = (Projection a -> Projection b -> Expr (Maybe Bool))

join' :: ProjectableGeneralizedZip pa pb pc
      => (qa -> QuerySimple (PlaceHolders pa, Projection a))
      -> (qb -> QuerySimple (PlaceHolders pb, Projection b))
      -> qa
      -> qb
      -> [JoinRestriction a b]
      -> Relation pc (a, b)
join' qL qR r0 r1 ons = relation' $ do
  (ph0, pj0) <- qL r0
  (ph1, pj1) <- qR r1
  sequence_ $ zipWith3 (\f a b -> on $ f a b) ons (repeat pj0) (repeat pj1)
  return $ (ph0 `generalizedZip` ph1, pj0 `projectZip` pj1)

inner' :: ProjectableGeneralizedZip pa pb pc
       => Relation pa a
       -> Relation pb b
       -> [JoinRestriction a b]
       -> Relation pc (a, b)
inner' =  join' query' query'

left'  :: ProjectableGeneralizedZip pa pb pc
       => Relation pa a
       -> Relation pb b
       -> [JoinRestriction a (Maybe b)]
       -> Relation pc (a, Maybe b)
left'  =  join' query' queryMaybe'

right' :: ProjectableGeneralizedZip pa pb pc
       => Relation pa a
       -> Relation pb b
       -> [JoinRestriction (Maybe a) b]
       -> Relation pc(Maybe a, b)
right' =  join' queryMaybe' query'

full'  :: ProjectableGeneralizedZip pa pb pc
       => Relation pa a
       -> Relation pb b
       -> [JoinRestriction (Maybe a) (Maybe b)]
       -> Relation pc (Maybe a, Maybe b)
full'  =  join' queryMaybe' queryMaybe'

join :: (qa -> QuerySimple (Projection a))
     -> (qb -> QuerySimple (Projection b))
     -> qa
     -> qb
     -> [JoinRestriction a b]
     -> Relation () (a, b)
join qL qR r0 r1 ons = relation $ do
  pj0 <- qL r0
  pj1 <- qR r1
  sequence_ $ zipWith3 (\f a b -> on $ f a b) ons (repeat pj0) (repeat pj1)
  return $ pj0 `projectZip` pj1

inner :: Relation () a
      -> Relation () b
      -> [JoinRestriction a b]
      -> Relation () (a, b)
inner =  join query query

left  :: Relation () a
      -> Relation () b
      -> [JoinRestriction a (Maybe b)]
      -> Relation () (a, Maybe b)
left  =  join query queryMaybe

right :: Relation () a
      -> Relation () b
      -> [JoinRestriction (Maybe a) b]
      -> Relation () (Maybe a, b)
right =  join queryMaybe query

full  :: Relation () a
      -> Relation () b
      -> [JoinRestriction (Maybe a) (Maybe b)]
      -> Relation () (Maybe a, Maybe b)
full  =  join queryMaybe queryMaybe

on' :: ([JoinRestriction a b] -> Relation pc (a, b))
    -> [JoinRestriction a b]
    -> Relation pc (a, b)
on' =  ($)

infixl 8 `inner'`, `left'`, `right'`, `full'`, `inner`, `left`, `right`, `full`, `on'`


sqlQualifyFromRelation :: Relation p r -> Qualify String
sqlQualifyFromRelation =  d  where
  d (SubQuery sub)    = return $ SubQuery.toSQL sub
  d (SimpleRel qp)    = Simple.toSQL qp
  d (AggregateRel qp) = Aggregate.toSQL qp

sqlFromRelation :: Relation p r -> String
sqlFromRelation =  evalQualifyPrime . sqlQualifyFromRelation

instance Show (Relation p r) where
  show = sqlFromRelation

width :: Relation p r -> Int
width =  SubQuery.width . subQueryFromRelation

nested :: Relation p r -> Relation p r
nested =  SubQuery . subQueryFromRelation

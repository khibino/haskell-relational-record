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

  -- nested, width,

  sqlFromRelation,

  -- * Query using relation
  query, query', queryMaybe, queryMaybe',

  -- * Direct style join
  JoinRestriction,
  rightPh, leftPh,
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

import Database.Relational.Query.Sub (SubQuery)
import qualified Database.Relational.Query.Sub as SubQuery


-- | Relation type with place-holder parameter 'p' and query result type 'r'.
data Relation p r = SubQuery SubQuery
                  | SimpleRel (SimpleQuery r)
                  | AggregateRel (AggregatedQuery r)


-- | Simple 'Relation' from 'Table'.
table :: Table r -> Relation () r
table =  SubQuery . SubQuery.fromTable


-- | Sub-query Qualify monad from relation.
subQueryQualifyFromRelation :: Relation p r -> Qualify SubQuery
subQueryQualifyFromRelation =  d  where
  d (SubQuery sub)    = return $ sub
  d (SimpleRel qp)    = Simple.toSubQuery qp
  d (AggregateRel qp) = Aggregate.toSubQuery qp

{-
-- | Sub-query from relation.
subQueryFromRelation :: Relation p r -> SubQuery
subQueryFromRelation =  evalQualifyPrime . subQueryQualifyFromRelation
-}

-- | Basic monadic join operation using 'MonadQuery'.
queryWithAttr :: MonadQualify Qualify m
              => NodeAttr -> Relation p r -> m (PlaceHolders p, Projection r)
queryWithAttr attr = addPlaceHolders . run where
  run rel = do
    q <- liftQualify $ do
      sq <- subQueryQualifyFromRelation rel
      qualifyQuery sq
    unsafeSubQuery attr q
  -- d (Relation q) = unsafeMergeAnotherQuery attr q

-- | Join subquery with place-holder parameter 'p'. query result is not 'Maybe'.
query' :: MonadQualify Qualify m => Relation p r -> m (PlaceHolders p, Projection r)
query' =  queryWithAttr Just'

-- | Join subquery. Query result is not 'Maybe'.
query :: MonadQualify Qualify m => Relation () r -> m (Projection r)
query =  fmap snd . query'

-- | Join subquery with place-holder parameter 'p'. Query result is 'Maybe'.
queryMaybe' :: MonadQualify Qualify m => Relation p r -> m (PlaceHolders p, Projection (Maybe r))
queryMaybe' pr =  do
  (ph, pj) <- queryWithAttr Maybe pr
  return (ph, Projection.just pj)

-- | Join subquery. Query result is 'Maybe'.
queryMaybe :: MonadQualify Qualify m => Relation () r -> m (Projection (Maybe r))
queryMaybe =  fmap snd . queryMaybe'

-- | Finalize 'QuerySimple' monad and generate 'Relation'.
relation :: QuerySimple (Projection r) -> Relation () r
relation =  SimpleRel

-- | Finalize 'QuerySimple' monad and generate 'Relation' with place-holder parameter 'p'.
relation' :: QuerySimple (PlaceHolders p, Projection r) -> Relation p r
relation' =  SimpleRel . fmap snd

-- | Finalize 'QueryAggregate' monad and geneate 'Relation'.
aggregateRelation :: QueryAggregate (Aggregation r) -> Relation () r
aggregateRelation =  AggregateRel

-- | Finalize 'QueryAggregate' monad and geneate 'Relation' with place-holder parameter 'p'.
aggregateRelation' :: QueryAggregate (PlaceHolders p, Aggregation r) -> Relation p r
aggregateRelation' =  AggregateRel . fmap snd


-- | Restriction function type for direct style join operator.
type JoinRestriction a b = Projection a -> Projection b -> Expr (Maybe Bool)

unsafeCastPlaceHolder :: Relation a r -> Relation b r
unsafeCastPlaceHolder =  d  where
  d (SubQuery q)      = SubQuery q
  d (SimpleRel qm)    = SimpleRel qm
  d (AggregateRel qm) = AggregateRel qm

-- | Simplify placeholder type applying left identity element.
rightPh :: Relation ((), p) r -> Relation p r
rightPh =  unsafeCastPlaceHolder

-- | Simplify placeholder type applying right identity element.
leftPh :: Relation (p, ()) r -> Relation p r
leftPh =  unsafeCastPlaceHolder

-- | Basic direct join operation with place-holder parameters.
join' :: (qa -> QuerySimple (PlaceHolders pa, Projection a))
      -> (qb -> QuerySimple (PlaceHolders pb, Projection b))
      -> qa
      -> qb
      -> [JoinRestriction a b]
      -> Relation (pa, pb) (a, b)
join' qL qR r0 r1 ons = relation' $ do
  (ph0, pj0) <- qL r0
  (ph1, pj1) <- qR r1
  sequence_ $ zipWith3 (\f a b -> on $ f a b) ons (repeat pj0) (repeat pj1)
  return $ (ph0 `projectZip` ph1, pj0 `projectZip` pj1)

-- | Direct inner join with place-holder parameters.
inner' :: Relation pa a            -- ^ Left query to join
       -> Relation pb b            -- ^ Right query to join
       -> [JoinRestriction a b]    -- ^ Join restrictions
       -> Relation (pa, pb) (a, b) -- ^ Result joined relation
inner' =  join' query' query'

-- | Direct left outer join with place-holder parameters.
left' :: Relation pa a                  -- ^ Left query to join
      -> Relation pb b                  -- ^ Right query to join
      -> [JoinRestriction a (Maybe b)]  -- ^ Join restrictions
      -> Relation (pa, pb) (a, Maybe b) -- ^ Result joined relation
left'  =  join' query' queryMaybe'

-- | Direct right outer join with place-holder parameters.
right' :: Relation pa a                 -- ^ Left query to join
       -> Relation pb b                 -- ^ Right query to join
       -> [JoinRestriction (Maybe a) b] -- ^ Join restrictions
       -> Relation (pa, pb)(Maybe a, b) -- ^ Result joined relation
right' =  join' queryMaybe' query'

-- | Direct full outer join with place-holder parameters.
full' :: Relation pa a                         -- ^ Left query to join
      -> Relation pb b                         -- ^ Right query to join
      -> [JoinRestriction (Maybe a) (Maybe b)] -- ^ Join restrictions
      -> Relation (pa, pb) (Maybe a, Maybe b)  -- ^ Result joined relation
full'  =  join' queryMaybe' queryMaybe'

-- | Basic direct join operation.
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

-- | Direct inner join.
inner :: Relation () a         -- ^ Left query to join
      -> Relation () b         -- ^ Right query to join
      -> [JoinRestriction a b] -- ^ Join restrictions
      -> Relation () (a, b)    -- ^ Result joined relation
inner =  join query query

-- | Direct left outer join.
left :: Relation () a                 -- ^ Left query to join
     -> Relation () b                 -- ^ Right query to join
     -> [JoinRestriction a (Maybe b)] -- ^ Join restrictions
     -> Relation () (a, Maybe b)      -- ^ Result joined relation
left  =  join query queryMaybe

-- | Direct right outer join.
right :: Relation () a                 -- ^ Left query to join
      -> Relation () b                 -- ^ Right query to join
      -> [JoinRestriction (Maybe a) b] -- ^ Join restrictions
      -> Relation () (Maybe a, b)      -- ^ Result joined relation
right =  join queryMaybe query

-- | Direct full outer join.
full :: Relation () a                         -- ^ Left query to join
     -> Relation () b                         -- ^ Right query to join
     -> [JoinRestriction (Maybe a) (Maybe b)] -- ^ Join restrictions
     -> Relation () (Maybe a, Maybe b)        -- ^ Result joined relation
full  =  join queryMaybe queryMaybe

-- | Apply restriction for direct join style.
on' :: ([JoinRestriction a b] -> Relation pc (a, b))
    -> [JoinRestriction a b]
    -> Relation pc (a, b)
on' =  ($)

infixl 8 `inner'`, `left'`, `right'`, `full'`, `inner`, `left`, `right`, `full`, `on'`


-- | SQL string with qualify computation from 'Relation'.
sqlQualifyFromRelation :: Relation p r -> Qualify String
sqlQualifyFromRelation =  d  where
  d (SubQuery sub)    = return $ SubQuery.toSQL sub
  d (SimpleRel qp)    = Simple.toSQL qp
  d (AggregateRel qp) = Aggregate.toSQL qp

-- | SQL string from 'Relation'.
sqlFromRelation :: Relation p r -> String
sqlFromRelation =  evalQualifyPrime . sqlQualifyFromRelation

instance Show (Relation p r) where
  show = sqlFromRelation

{-
-- | Get projection width from 'Relation'.
width :: Relation p r -> Int
width =  SubQuery.width . subQueryFromRelation

-- | Finalize internal Query monad.
nested :: Relation p r -> Relation p r
nested =  SubQuery . subQueryFromRelation
-}

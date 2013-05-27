
module Database.Relational.Query.Relation (
  table,
  relation, relation',
  aggregateRelation, aggregateRelation',

  query, query', queryMaybe, queryMaybe', from,

  PrimeRelation, Relation,

  inner', left', right', full',
  inner, left, right, full,
  on',

  sqlFromRelation,

  subQueryFromRelation,

  nested, width
  ) where

import Database.Relational.Query.Monad.Class (MonadQuery (on))
import qualified Database.Relational.Query.Monad.Unsafe as UnsafeMonadQuery
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


data PrimeRelation p r = SubQuery SubQuery
                       | SimpleRel (SimpleQuery r)
                       | AggregateRel (AggregatedQuery r)

type Relation r = PrimeRelation () r


table :: Table r -> Relation r
table =  SubQuery . SubQuery.fromTable

from :: Table r -> Relation r
from =  table


subQueryFromRelation :: PrimeRelation p r -> SubQuery
subQueryFromRelation =  d  where
  d (SubQuery sub)    = sub
  d (SimpleRel qp)    = Simple.toSubQuery qp
  d (AggregateRel qp) = Aggregate.toSubQuery qp

queryWithAttr :: MonadQuery m => NodeAttr -> PrimeRelation p r -> m (PlaceHolders p, Projection r)
queryWithAttr attr = addPlaceHolders . q where
  q = UnsafeMonadQuery.unsafeSubQuery attr . subQueryFromRelation
  -- d (PrimeRelation q) = UnsafeMonadQuery.unsafeMergeAnotherQuery attr q

query' :: MonadQuery m => PrimeRelation p r -> m (PlaceHolders p, Projection r)
query' =  queryWithAttr Just'

query :: MonadQuery m => Relation r -> m (Projection r)
query =  fmap snd . query'

queryMaybe' :: MonadQuery m => PrimeRelation p r -> m (PlaceHolders p, Projection (Maybe r))
queryMaybe' pr =  do
  (ph, pj) <- queryWithAttr Maybe pr
  return (ph, Projection.just pj)

queryMaybe :: MonadQuery m => PrimeRelation p r -> m (Projection (Maybe r))
queryMaybe =  fmap snd . queryMaybe'

relation :: QuerySimple (Projection r) -> PrimeRelation p r
relation =  SimpleRel

relation' :: QuerySimple (PlaceHolders p, Projection r) -> PrimeRelation p r
relation' =  SimpleRel . fmap snd

aggregateRelation :: QueryAggregate (Aggregation r) -> PrimeRelation p r
aggregateRelation =  AggregateRel

aggregateRelation' :: QueryAggregate (PlaceHolders p, Aggregation r) -> PrimeRelation p r
aggregateRelation' =  AggregateRel . fmap snd


type JoinRestriction a b = (Projection a -> Projection b -> Expr (Maybe Bool))

join' :: ProjectableGeneralizedZip pa pb pc
      => (qa -> QuerySimple (PlaceHolders pa, Projection a))
      -> (qb -> QuerySimple (PlaceHolders pb, Projection b))
      -> qa
      -> qb
      -> [JoinRestriction a b]
      -> PrimeRelation pc (a, b)
join' qL qR r0 r1 ons = relation' $ do
  (ph0, pj0) <- qL r0
  (ph1, pj1) <- qR r1
  sequence_ $ zipWith3 (\f a b -> on $ f a b) ons (repeat pj0) (repeat pj1)
  return $ (ph0 `generalizedZip` ph1, pj0 `projectZip` pj1)

inner' :: ProjectableGeneralizedZip pa pb pc
       => PrimeRelation pa a
       -> PrimeRelation pb b
       -> [JoinRestriction a b]
       -> PrimeRelation pc (a, b)
inner' =  join' query' query'

left'  :: ProjectableGeneralizedZip pa pb pc
       => PrimeRelation pa a
       -> PrimeRelation pb b
       -> [JoinRestriction a (Maybe b)]
       -> PrimeRelation pc (a, Maybe b)
left'  =  join' query' queryMaybe'

right' :: ProjectableGeneralizedZip pa pb pc
       => PrimeRelation pa a
       -> PrimeRelation pb b
       -> [JoinRestriction (Maybe a) b]
       -> PrimeRelation pc(Maybe a, b)
right' =  join' queryMaybe' query'

full'  :: ProjectableGeneralizedZip pa pb pc
       => PrimeRelation pa a
       -> PrimeRelation pb b
       -> [JoinRestriction (Maybe a) (Maybe b)]
       -> PrimeRelation pc (Maybe a, Maybe b)
full'  =  join' queryMaybe' queryMaybe'

join :: (qa -> QuerySimple (Projection a))
     -> (qb -> QuerySimple (Projection b))
     -> qa
     -> qb
     -> [JoinRestriction a b]
     -> Relation (a, b)
join qL qR r0 r1 ons = relation $ do
  pj0 <- qL r0
  pj1 <- qR r1
  sequence_ $ zipWith3 (\f a b -> on $ f a b) ons (repeat pj0) (repeat pj1)
  return $ pj0 `projectZip` pj1

inner :: Relation a
      -> Relation b
      -> [JoinRestriction a b]
      -> Relation (a, b)
inner =  join query query

left  :: Relation a
      -> Relation b
      -> [JoinRestriction a (Maybe b)]
      -> Relation (a, Maybe b)
left  =  join query queryMaybe

right :: Relation a
      -> Relation b
      -> [JoinRestriction (Maybe a) b]
      -> Relation (Maybe a, b)
right =  join queryMaybe query

full  :: Relation a
      -> Relation b
      -> [JoinRestriction (Maybe a) (Maybe b)]
      -> Relation (Maybe a, Maybe b)
full  =  join queryMaybe queryMaybe

on' :: ([JoinRestriction a b] -> PrimeRelation pc (a, b))
    -> [JoinRestriction a b]
    -> PrimeRelation pc (a, b)
on' =  ($)

infixl 8 `inner'`, `left'`, `right'`, `full'`, `inner`, `left`, `right`, `full`, `on'`


sqlFromRelation :: PrimeRelation p r -> String
sqlFromRelation =  d  where
  d (SubQuery sub)    = SubQuery.toSQL sub
  d (SimpleRel qp)    = Simple.toSQL qp
  d (AggregateRel qp) = Aggregate.toSQL qp

instance Show (PrimeRelation p r) where
  show = sqlFromRelation

width :: PrimeRelation p r -> Int
width =  SubQuery.width . subQueryFromRelation

nested :: PrimeRelation p r -> PrimeRelation p r
nested =  SubQuery . subQueryFromRelation

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

  table, derivedRelation, tableOf,
  relation, relation',
  aggregateRelation, aggregateRelation',

  UniqueRelation,
  unsafeUnique, unUnique,

  uniqueRelation', aggregatedUnique,

  -- * Query using relation
  query, queryMaybe, queryList, queryList', queryScalar, queryScalar',
  uniqueQuery', uniqueQueryMaybe',

  -- * Direct style join
  JoinRestriction,
  inner', left', right', full',
  inner, left, right, full,
  on',

  -- * Relation append
  union, except, intersect,
  unionAll, exceptAll, intersectAll,

  union', except', intersect',
  unionAll', exceptAll', intersectAll',
  ) where

import Control.Applicative ((<$>))

import Database.Relational.Query.Context (Flat, Aggregated)
import Database.Relational.Query.Monad.BaseType
  (ConfigureQuery, qualifyQuery,
   Relation, unsafeTypeRelation, untypeRelation)
import Database.Relational.Query.Monad.Class
  (MonadQualify (liftQualify), MonadQuery (query', queryMaybe'), on)
import Database.Relational.Query.Monad.Simple (QuerySimple, SimpleQuery)
import qualified Database.Relational.Query.Monad.Simple as Simple
import Database.Relational.Query.Monad.Aggregate (QueryAggregate, AggregatedQuery)
import qualified Database.Relational.Query.Monad.Aggregate as Aggregate
import Database.Relational.Query.Monad.Unique (QueryUnique, unsafeUniqueSubQuery)
import qualified Database.Relational.Query.Monad.Unique as Unique

import Database.Relational.Query.Component (Duplication (Distinct, All))
import Database.Relational.Query.Table (Table, TableDerivable, derivedTable)
import Database.Relational.Query.Internal.Sub (NodeAttr(Just', Maybe))
import Database.Relational.Query.Sub (SubQuery)
import qualified Database.Relational.Query.Sub as SubQuery

import Database.Relational.Query.Scalar (ScalarDegree)
import Database.Relational.Query.Pi (Pi)
import Database.Relational.Query.Projection
  (Projection, ListProjection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Projectable
  (PlaceHolders, unitPlaceHolder, unsafeAddPlaceHolders, unsafePlaceHolders, projectZip)
import Database.Relational.Query.ProjectableExtended ((!))


-- | Simple 'Relation' from 'Table'.
table :: Table r -> Relation () r
table =  unsafeTypeRelation . return . SubQuery.fromTable

-- | Infered 'Relation'.
derivedRelation :: TableDerivable r => Relation () r
derivedRelation =  table derivedTable

-- | Interface to derive 'Table' type object.
tableOf :: TableDerivable r => Relation () r -> Table r
tableOf =  const derivedTable

placeHoldersFromRelation :: Relation p r -> PlaceHolders p
placeHoldersFromRelation =  const unsafePlaceHolders

-- | Join sub-query. Query result is not 'Maybe'.
query :: (MonadQualify ConfigureQuery m, MonadQuery m)
      => Relation () r
      -> m (Projection Flat r)
query =  fmap snd . query'

-- | Join sub-query. Query result is 'Maybe'.
--   The combinations of 'query' and 'queryMaybe' express
--   inner joins, left outer joins, right outer joins, and full outer joins.
--   Here is an example of a right outer join:
--
-- @
--   outerJoin = relation $ do
--     e <- queryMaybe employee
--     d <- query department
--     on $ e ?! E.deptId' .=. just (d ! D.deptId')
--     return $ (,) |$| e |*| d
-- @
queryMaybe :: (MonadQualify ConfigureQuery m, MonadQuery m)
           => Relation () r
           -> m (Projection Flat (Maybe r))
queryMaybe =  fmap snd . queryMaybe'

queryList0 :: MonadQualify ConfigureQuery m => Relation p r -> m (ListProjection (Projection c) r)
queryList0 =  liftQualify
              . fmap Projection.unsafeListFromSubQuery
              . untypeRelation

-- | List sub-query, for /IN/ and /EXIST/ with place-holder parameter 'p'.
queryList' :: MonadQualify ConfigureQuery m
           => Relation p r
           -> m (PlaceHolders p, ListProjection (Projection c) r)
queryList' rel = do
  ql <- queryList0 rel
  return (placeHoldersFromRelation rel, ql)

-- | List sub-query, for /IN/ and /EXIST/.
queryList :: MonadQualify ConfigureQuery m
          => Relation () r
          -> m (ListProjection (Projection c) r)
queryList =  queryList0

addUnitPH :: Functor f => f t -> f (PlaceHolders (), t)
addUnitPH =  ((,) unitPlaceHolder <$>)

-- | Finalize 'QuerySimple' monad and generate 'Relation' with place-holder parameter 'p'.
relation' :: SimpleQuery p r -> Relation p r
relation' =  unsafeTypeRelation . Simple.toSubQuery

-- | Finalize 'QuerySimple' monad and generate 'Relation'.
relation :: QuerySimple (Projection Flat r) -> Relation () r
relation =  relation' . addUnitPH

-- | Finalize 'QueryAggregate' monad and geneate 'Relation' with place-holder parameter 'p'.
aggregateRelation' :: AggregatedQuery p r -> Relation p r
aggregateRelation' =  unsafeTypeRelation . Aggregate.toSubQuery

-- | Finalize 'QueryAggregate' monad and geneate 'Relation'.
aggregateRelation :: QueryAggregate (Projection Aggregated r) -> Relation () r
aggregateRelation =  aggregateRelation' . addUnitPH


-- | Restriction function type for direct style join operator.
type JoinRestriction a b = Projection Flat a -> Projection Flat b -> Projection Flat (Maybe Bool)

-- | Basic direct join operation with place-holder parameters.
join' :: (qa -> QuerySimple (PlaceHolders pa, Projection Flat a))
      -> (qb -> QuerySimple (PlaceHolders pb, Projection Flat b))
      -> qa
      -> qb
      -> [JoinRestriction a b]
      -> Relation (pa, pb) (a, b)
join' qL qR r0 r1 rs = relation' $ do
  (ph0, pj0) <- qL r0
  (ph1, pj1) <- qR r1
  sequence_ [ on $ f pj0 pj1 | f <- rs ]
  return (ph0 `projectZip` ph1, pj0 `projectZip` pj1)

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
join_ :: (qa -> QuerySimple (Projection Flat a))
      -> (qb -> QuerySimple (Projection Flat b))
      -> qa
      -> qb
      -> [JoinRestriction a b]
      -> Relation () (a, b)
join_ qL qR r0 r1 rs = relation $ do
  pj0 <- qL r0
  pj1 <- qR r1
  sequence_ [ on $ f pj0 pj1 | f <- rs ]
  return $ pj0 `projectZip` pj1

-- | Direct inner join.
inner :: Relation () a         -- ^ Left query to join
      -> Relation () b         -- ^ Right query to join
      -> [JoinRestriction a b] -- ^ Join restrictions
      -> Relation () (a, b)    -- ^ Result joined relation
inner =  join_ query query

-- | Direct left outer join.
left :: Relation () a                 -- ^ Left query to join
     -> Relation () b                 -- ^ Right query to join
     -> [JoinRestriction a (Maybe b)] -- ^ Join restrictions
     -> Relation () (a, Maybe b)      -- ^ Result joined relation
left  =  join_ query queryMaybe

-- | Direct right outer join.
right :: Relation () a                 -- ^ Left query to join
      -> Relation () b                 -- ^ Right query to join
      -> [JoinRestriction (Maybe a) b] -- ^ Join restrictions
      -> Relation () (Maybe a, b)      -- ^ Result joined relation
right =  join_ queryMaybe query

-- | Direct full outer join.
full :: Relation () a                         -- ^ Left query to join
     -> Relation () b                         -- ^ Right query to join
     -> [JoinRestriction (Maybe a) (Maybe b)] -- ^ Join restrictions
     -> Relation () (Maybe a, Maybe b)        -- ^ Result joined relation
full  =  join_ queryMaybe queryMaybe

-- | Apply restriction for direct join style.
on' :: ([JoinRestriction a b] -> Relation pc (a, b))
    -> [JoinRestriction a b]
    -> Relation pc (a, b)
on' =  ($)

infixl 8 `inner'`, `left'`, `right'`, `full'`, `inner`, `left`, `right`, `full`, `on'`

unsafeLiftAppend :: (SubQuery -> SubQuery -> SubQuery)
           -> Relation p a
           -> Relation q a
           -> Relation r a
unsafeLiftAppend op a0 a1 = unsafeTypeRelation $ do
  s0 <- untypeRelation a0
  s1 <- untypeRelation a1
  return $ s0 `op` s1

liftAppend :: (SubQuery -> SubQuery -> SubQuery)
           -> Relation () a
           -> Relation () a
           -> Relation () a
liftAppend = unsafeLiftAppend

-- | Union of two relations.
union     :: Relation () a -> Relation () a -> Relation () a
union     =  liftAppend $ SubQuery.union Distinct

-- | Union of two relations. Not distinct.
unionAll  :: Relation () a -> Relation () a -> Relation () a
unionAll  =  liftAppend $ SubQuery.union All

-- | Subtraction of two relations.
except    :: Relation () a -> Relation () a -> Relation () a
except    =  liftAppend $ SubQuery.except Distinct

-- | Subtraction of two relations. Not distinct.
exceptAll :: Relation () a -> Relation () a -> Relation () a
exceptAll =  liftAppend $ SubQuery.except All

-- | Intersection of two relations.
intersect :: Relation () a -> Relation () a -> Relation () a
intersect =  liftAppend $ SubQuery.intersect Distinct

-- | Intersection of two relations. Not distinct.
intersectAll :: Relation () a -> Relation () a -> Relation () a
intersectAll =  liftAppend $ SubQuery.intersect All

liftAppend' :: (SubQuery -> SubQuery -> SubQuery)
            -> Relation p a
            -> Relation q a
            -> Relation (p, q) a
liftAppend' = unsafeLiftAppend

-- | Union of two relations with place-holder parameters.
union'     :: Relation p a -> Relation q a -> Relation (p, q) a
union'     =  liftAppend' $ SubQuery.union Distinct

-- | Union of two relations with place-holder parameters. Not distinct.
unionAll' :: Relation p a -> Relation q a -> Relation (p, q) a
unionAll'  =  liftAppend' $ SubQuery.union All

-- | Subtraction of two relations with place-holder parameters.
except'    :: Relation p a -> Relation q a -> Relation (p, q) a
except'    =  liftAppend' $ SubQuery.except Distinct

-- | Subtraction of two relations with place-holder parameters. Not distinct.
exceptAll' :: Relation p a -> Relation q a -> Relation (p, q) a
exceptAll' =  liftAppend' $ SubQuery.except All

-- | Intersection of two relations with place-holder parameters.
intersect' :: Relation p a -> Relation q a -> Relation (p, q) a
intersect' =  liftAppend' $ SubQuery.intersect Distinct

-- | Intersection of two relations with place-holder parameters. Not distinct.
intersectAll' :: Relation p a -> Relation q a -> Relation (p, q) a
intersectAll' =  liftAppend' $ SubQuery.intersect All

infixl 7 `union`, `except`, `unionAll`, `exceptAll`
infixl 8 `intersect`, `intersectAll`
infixl 7 `union'`, `except'`, `unionAll'`, `exceptAll'`
infixl 8 `intersect'`, `intersectAll'`

{-
-- | Get projection width from 'Relation'.
width :: Relation p r -> Int
width =  SubQuery.width . subQueryFromRelation
-}

-- | Unique relation type to compose scalar queries.
newtype UniqueRelation p c r =  Unique (Relation p r)

-- | Unsafely specify unique relation.
unsafeUnique :: Relation p r -> UniqueRelation p c r
unsafeUnique =  Unique

-- | Discard unique attribute.
unUnique :: UniqueRelation p c r -> Relation p r
unUnique (Unique r) = r

-- | Basic monadic join operation using 'MonadQuery'.
uniqueQueryWithAttr :: NodeAttr
                    -> UniqueRelation p c r
                    -> QueryUnique (PlaceHolders p, Projection c r)
uniqueQueryWithAttr attr = unsafeAddPlaceHolders . run where
  run rel = do
    q <- liftQualify $ do
      sq <- untypeRelation (unUnique rel)
      qualifyQuery sq
    Projection.unsafeChangeContext <$> unsafeUniqueSubQuery attr q

-- | Join unique sub-query with place-holder parameter 'p'.
uniqueQuery' :: UniqueRelation p c r
             -> QueryUnique (PlaceHolders p, Projection c r)
uniqueQuery' = uniqueQueryWithAttr Just'

-- | Join unique sub-query with place-holder parameter 'p'. Query result is 'Maybe'.
uniqueQueryMaybe' :: UniqueRelation p c r
                  -> QueryUnique (PlaceHolders p, Projection c (Maybe r))
uniqueQueryMaybe' pr =  do
  (ph, pj) <- uniqueQueryWithAttr Maybe pr
  return (ph, Projection.just pj)

-- | Finalize 'QueryUnique' monad and generate 'UniqueRelation'.
uniqueRelation' :: QueryUnique (PlaceHolders p, Projection c r) -> UniqueRelation p c r
uniqueRelation' =  unsafeUnique . unsafeTypeRelation . Unique.toSubQuery

-- | Aggregated 'UniqueRelation'.
aggregatedUnique :: Relation ph r
                 -> Pi r a
                 -> (Projection Flat a -> Projection Aggregated b)
                 -> UniqueRelation ph Flat b
aggregatedUnique rel k ag = unsafeUnique . aggregateRelation' $ do
  (ph, a) <- query' rel
  return (ph, ag $ a ! k)

-- | Scalar sub-query with place-holder parameter 'p'.
queryScalar' :: (MonadQualify ConfigureQuery m, ScalarDegree r)
             => UniqueRelation p c r
             -> m (PlaceHolders p, Projection c (Maybe r))
queryScalar' ur =
  unsafeAddPlaceHolders . liftQualify $
  Projection.unsafeFromScalarSubQuery <$> untypeRelation (unUnique ur)

-- | Scalar sub-query.
queryScalar :: (MonadQualify ConfigureQuery m, ScalarDegree r)
            => UniqueRelation () c r
            -> m (Projection c (Maybe r))
queryScalar =  fmap snd . queryScalar'

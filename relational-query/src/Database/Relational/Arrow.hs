{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Database.Relational.Arrow
-- Copyright   : 2015-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines arrow version combinators which
-- improves type-safty on building queries.
-- Referencing the local projections may cause to break
-- the result query.
-- It is possible to controls injection of previous local projections
-- by restricting domain type of arrow. This idea is imported from Opaleye:
--
--   * <https://github.com/tomjaguarpaw/haskell-opaleye>
--   * <https://github.com/khibino/haskell-relational-record/issues/19>
--
-- Importing this module instead of "Database.Relational.Query" enables
-- to build query using arrow combinators.
module Database.Relational.Arrow (
  module Database.Relational,

  all', distinct,

  query, queryMaybe, query', queryMaybe',
  queryList, queryList', queryExists, queryExists', queryListU, queryListU',
  queryScalar, queryScalar', queryScalarU, queryScalarU',

  uniqueQuery', uniqueQueryMaybe',

  on, wheres, having, groupBy, placeholder,

  relation, relation', aggregateRelation, aggregateRelation',

  uniqueRelation',

  groupBy', key, key', set, bkey, rollup, cube, groupingSets,

  orderBy', orderBy, asc, desc,

  partitionBy, over,

  assign,

  derivedUpdate', derivedUpdate,
  derivedInsertValue', derivedInsertValue,
  derivedDelete', derivedDelete,

  QueryA,

  QuerySimple, QueryAggregate, QueryUnique,

  AggregatingSet, AggregatingSetList, AggregatingPowerSet,

  Orderings, Window, Assignings,

  AssignStatement, Register, RestrictedStatement,
  ) where

import Control.Category (Category)
import Control.Arrow (Arrow, Kleisli (..))

import Database.Record

import Database.Relational hiding
  (all', distinct,
   query, queryMaybe, query', queryMaybe',
   queryList, queryList', queryScalar, queryScalar',
   uniqueQuery', uniqueQueryMaybe',
   on, wheres, having, groupBy, placeholder,
   relation, relation', aggregateRelation, aggregateRelation', uniqueRelation',
   groupBy', key, key', set, bkey, rollup, cube, groupingSets,
   orderBy', orderBy, asc, desc, partitionBy, over,
   derivedUpdate', derivedUpdate,
   derivedInsertValue', derivedInsertValue,
   derivedDelete', derivedDelete,
   QuerySimple, QueryAggregate, QueryUnique, Window, Register)
import qualified Database.Relational as Monadic
import Database.Relational.Projection (ListProjection)
import qualified Database.Relational.Monad.Trans.Aggregating as Monadic
import qualified Database.Relational.Monad.Trans.Ordering as Monadic
import qualified Database.Relational.Monad.Trans.Assigning as Monadic


-- | Arrow to build queries.
newtype QueryA m a b = QueryA (Kleisli m a b) deriving (Category, Arrow)

queryA :: (a -> m b) -> QueryA m a b
queryA = QueryA . Kleisli

runQueryA :: QueryA m a b -> a -> m b
runQueryA (QueryA k) = runKleisli k

runAofM :: (m b -> c) -> QueryA m () b -> c
runAofM = (. (`runQueryA` ()))

-- | Arrow type corresponding to 'Monadic.QuerySimple'
type QuerySimple    = QueryA Monadic.QuerySimple

-- | Arrow type corresponding to 'Monadic.QueryAggregate'
type QueryAggregate = QueryA Monadic.QueryAggregate

-- | Arrow type corresponding to 'Monadic.QueryUnique'
type QueryUnique = QueryA Monadic.QueryUnique

-- | Arrow type corresponding to 'Monadic.AggregatingSet'
type AggregatingSet = QueryA Monadic.AggregatingSet

-- | Arrow type corresponding to 'Monadic.AggregatingSetList'
type AggregatingSetList = QueryA Monadic.AggregatingSetList

-- | Arrow type corresponding to 'Monadic.AggregatingPowerSet'
type AggregatingPowerSet = QueryA Monadic.AggregatingPowerSet

-- | Arrow type corresponding to 'Monadic.Orderings'
type Orderings c m = QueryA (Monadic.Orderings c m)

-- | Arrow type corresponding to 'Monadic.Window'
type Window c = QueryA (Monadic.Window c)

-- | Arrow type corresponding to 'Monadic.Assignings'
type Assignings r m = QueryA (Monadic.Assignings r m)

-- | Arrow type corresponding to 'Monadic.AssignStatement'
type AssignStatement r a = Assignings r Restrict (Projection Flat r) a

-- | Arrow type corresponding to 'Monadic.Register'
type Register r a = QueryA (Monadic.Register r) () a

-- | Arrow type corresponding to 'Monadic.RestrictedStatement'
type RestrictedStatement r a = QueryA Monadic.Restrict (Projection Flat r) a


-- | Same as 'Monadic.all''. Arrow version.
all' :: MonadQuery m => QueryA m () ()
all' = queryA $ \() -> Monadic.all'

-- | Same as 'Monadic.distinct'. Arrow version.
distinct :: MonadQuery m => QueryA m () ()
distinct = queryA $ \() -> Monadic.distinct

-- | Same as 'Monadic.query'. Arrow version.
--   The result arrow is not injected by local projections.
query :: (MonadQualify ConfigureQuery m, MonadQuery m)
      => Relation () r -> QueryA m () (Projection Flat r)
query r = queryA $ \() -> Monadic.query r

-- | Same as 'Monadic.queryMaybe'. Arrow version.
--   The result arrow is not injected by any local projections.
queryMaybe :: (MonadQualify ConfigureQuery m, MonadQuery m)
           => Relation () r -> QueryA m () (Projection Flat (Maybe r))
queryMaybe r = queryA $ \() -> Monadic.queryMaybe r

-- | Same as 'Monadic.query''. Arrow version.
--   The result arrow is not injected by any local projections.
query' :: (MonadQualify ConfigureQuery m, MonadQuery m)
       => Relation p r -> QueryA m () (PlaceHolders p, Projection Flat r)
query' r = queryA $ \() -> Monadic.query' r

-- | Same as 'Monadic.queryMaybe''. Arrow version.
--   The result arrow is not injected by any local projections.
queryMaybe' :: (MonadQualify ConfigureQuery m, MonadQuery m)
            => Relation p r -> QueryA m () (PlaceHolders p, Projection Flat (Maybe r))
queryMaybe' r = queryA $ \() -> Monadic.queryMaybe' r

unsafeQueryList :: MonadQualify ConfigureQuery m
            => (a -> Relation () r)
            -> QueryA m a (ListProjection (Projection c) r)
unsafeQueryList rf = queryA $ Monadic.queryList . rf

unsafeQueryList' :: MonadQualify ConfigureQuery m
             => (a -> Relation p r)
             -> QueryA m a (PlaceHolders p, ListProjection (Projection c) r)
unsafeQueryList' rf = queryA $ Monadic.queryList' . rf

-- | Same as 'Monadic.queryList'. Arrow version.
--   The result arrow is designed to be injected by local projections.
queryList :: MonadQualify ConfigureQuery m
          => (Projection c a -> Relation () r)
          -> QueryA m (Projection c a) (ListProjection (Projection c) r)
queryList = unsafeQueryList

-- | Same as 'Monadic.queryList''. Arrow version.
--   The result arrow is designed to be injected by local projections.
queryList' :: MonadQualify ConfigureQuery m
           => (Projection c a -> Relation p r)
           -> QueryA m (Projection c a) (PlaceHolders p, ListProjection (Projection c) r)
queryList' = unsafeQueryList'

-- | Same as 'Monadic.queryList' to pass this result to 'exists' operator. Arrow version.
--   The result arrow is designed to be injected by local projections.
queryExists :: MonadQualify ConfigureQuery m
          => (Projection c a -> Relation () r)
          -> QueryA m (Projection c a) (ListProjection (Projection Exists) r)
queryExists = unsafeQueryList

-- | Same as 'Monadic.queryList'' to pass this result to 'exists' operator. Arrow version.
--   The result arrow is designed to be injected by local projections.
queryExists' :: MonadQualify ConfigureQuery m
           => (Projection c a -> Relation p r)
           -> QueryA m (Projection c a) (PlaceHolders p, ListProjection (Projection Exists) r)
queryExists' = unsafeQueryList'

-- | Same as 'Monadic.queryList'. Arrow version.
--   Useful for no reference cases to local projections.
queryListU :: MonadQualify ConfigureQuery m
           => Relation () r
           -> QueryA m () (ListProjection (Projection c) r)
queryListU r = unsafeQueryList $ \() -> r

-- | Same as 'Monadic.queryList''. Arrow version.
--   Useful for no reference cases to local projections.
queryListU' :: MonadQualify ConfigureQuery m
           => Relation p r
           -> QueryA m () (PlaceHolders p, ListProjection (Projection c) r)
queryListU' r = unsafeQueryList' $ \() -> r

unsafeQueryScalar :: (MonadQualify ConfigureQuery m, ScalarDegree r)
                  => (a -> UniqueRelation () c r)
                  -> QueryA m a (Projection c (Maybe r))
unsafeQueryScalar rf = queryA $ Monadic.queryScalar . rf

unsafeQueryScalar' :: (MonadQualify ConfigureQuery m, ScalarDegree r)
                   => (a -> UniqueRelation p c r)
                   -> QueryA m a (PlaceHolders p, Projection c (Maybe r))
unsafeQueryScalar' rf = queryA $ Monadic.queryScalar' . rf

-- | Same as 'Monadic.queryScalar'. Arrow version.
--   The result arrow is designed to be injected by any local projection.
queryScalar :: (MonadQualify ConfigureQuery m, ScalarDegree r)
            => (Projection c a -> UniqueRelation () c r)
            -> QueryA m (Projection c a) (Projection c (Maybe r))
queryScalar = unsafeQueryScalar

-- | Same as 'Monadic.queryScalar''. Arrow version.
--   The result arrow is designed to be injected by any local projection.
queryScalar' :: (MonadQualify ConfigureQuery m, ScalarDegree r)
             => (Projection c a -> UniqueRelation p c r)
             -> QueryA m (Projection c a) (PlaceHolders p, Projection c (Maybe r))
queryScalar' = unsafeQueryScalar'

-- | Same as 'Monadic.queryScalar'. Arrow version.
--   Useful for no reference cases to local projections.
queryScalarU :: (MonadQualify ConfigureQuery m, ScalarDegree r)
            => UniqueRelation () c r
            -> QueryA m () (Projection c (Maybe r))
queryScalarU r = unsafeQueryScalar $ \() -> r

-- | Same as 'Monadic.queryScalar''. Arrow version.
--   Useful for no reference cases to local projections.
queryScalarU' :: (MonadQualify ConfigureQuery m, ScalarDegree r)
             => UniqueRelation p c r
             -> QueryA m () (PlaceHolders p, Projection c (Maybe r))
queryScalarU' r = unsafeQueryScalar' $ \() -> r

-- | Same as 'Monadic.uniqueQuery''. Arrow version.
--   The result arrow is not injected by local projections.
uniqueQuery' :: UniqueRelation p c r
             -> QueryA Monadic.QueryUnique () (PlaceHolders p, Projection c r)
uniqueQuery' r = queryA $ \() -> Monadic.uniqueQuery' r

-- | Same as 'Monadic.uniqueQueryMaybe''. Arrow version.
--   The result arrow is not injected by local projections.
uniqueQueryMaybe' :: UniqueRelation p c r
                  -> QueryA Monadic.QueryUnique () (PlaceHolders p, Projection c (Maybe r))
uniqueQueryMaybe' r = queryA $ \() -> Monadic.uniqueQueryMaybe' r

-- | Same as 'Monadic.on'. Arrow version.
--   The result arrow is designed to be injected by local conditional flat-projections.
on :: MonadQuery m
   => QueryA m (Projection Flat (Maybe Bool)) ()
on = queryA Monadic.on

-- | Same as 'Monadic.wheres'. Arrow version.
--   The result arrow is designed to be injected by local conditional flat-projections.
wheres :: MonadRestrict Flat m
       => QueryA m (Projection Flat (Maybe Bool)) ()
wheres = queryA Monadic.wheres

-- | Same as 'Monadic.having'. Arrow version.
--   The result arrow is designed to be injected by local conditional aggregated-projections.
having :: MonadRestrict Aggregated m
       => QueryA m (Projection Aggregated (Maybe Bool)) ()
having = queryA Monadic.having

-- | Same as 'Monadic.groupBy'. Arrow version.
--   The result arrow is designed to be injected by local flat-projections.
groupBy :: MonadAggregate m
        => QueryA m (Projection Flat r) (Projection Aggregated r)
groupBy = queryA Monadic.groupBy

-- | Same as 'Monadic.placeholder'. Arrow version.
--   The result arrow is designed to be injected by locally built arrow using placeholders.
placeholder :: (PersistableWidth t, SqlProjectable p, Monad m)
            => QueryA m (QueryA m (p t) a) (PlaceHolders t, a)
placeholder = queryA $ Monadic.placeholder . runQueryA

-- | Same as 'Monadic.relation'.
--   Finalize query-building arrow instead of query-building monad.
relation :: QuerySimple () (Projection Flat r)
         -> Relation () r
relation = runAofM Monadic.relation

-- | Same as 'Monadic.relation''.
--   Finalize query-building arrow instead of query-building monad.
relation' :: QuerySimple () (PlaceHolders p, Projection Flat r)
          -> Relation p r
relation' = runAofM Monadic.relation'

-- | Same as 'Monadic.aggregateRelation'.
--   Finalize query-building arrow instead of query-building monad.
aggregateRelation :: QueryAggregate () (Projection Aggregated r)
                  -> Relation () r
aggregateRelation = runAofM Monadic.aggregateRelation

-- | Same as 'Monadic.aggregateRelation''.
--   Finalize query-building arrow instead of query-building monad.
aggregateRelation' :: QueryAggregate () (PlaceHolders p, Projection Aggregated r)
                   -> Relation p r
aggregateRelation' = runAofM Monadic.aggregateRelation'

-- | Same as 'Monadic.uniqueRelation''.
--   Finalize query-building arrow instead of query-building monad.
uniqueRelation' :: QueryUnique () (PlaceHolders p, Projection c r)
                -> UniqueRelation p c r
uniqueRelation' = runAofM Monadic.uniqueRelation'

-- | Same as 'Monadic.groupBy''.
--   This arrow is designed to be injected by local 'AggregateKey'.
groupBy' :: MonadAggregate m => QueryA m (AggregateKey (Projection Aggregated r)) (Projection Aggregated r)
groupBy' = queryA Monadic.groupBy'

-- | Same as 'Monadic.key'.
--   This arrow is designed to be injected by local flat-projections.
key :: AggregatingSet (Projection Flat r) (Projection Aggregated (Maybe r))
key = queryA Monadic.key

-- | Same as 'Monadic.key''.
--   This arrow is designed to be injected by local 'AggregteKey'.
key' :: AggregatingSet (AggregateKey a) a
key' = queryA Monadic.key'

-- | Same as 'Monadic.set'.
--   This arrow is designed to be injected by locally built 'AggregtingSet' arrow.
set :: AggregatingSetList (AggregatingSet () a) a
set = queryA $ runAofM Monadic.set

-- | Same as 'Monadic.bkey'.
--   This arrow is designed to be injected by local flat-projections.
bkey :: AggregatingPowerSet (Projection Flat r) (Projection Aggregated (Maybe r))
bkey = queryA Monadic.bkey

-- | Same as 'Monadic.rollup'.
--   Finalize locally built 'AggregatingPowerSet'.
rollup :: AggregatingPowerSet () a -> AggregateKey a
rollup = runAofM Monadic.rollup

-- | Same as 'Monadic.cube'.
--   Finalize locally built 'AggregatingPowerSet'.
cube :: AggregatingPowerSet () a -> AggregateKey a
cube = runAofM Monadic.cube

-- | Same as 'Monadic.groupingSets'.
--   Finalize locally built 'AggregatingSetList'.
groupingSets :: AggregatingSetList () a -> AggregateKey a
groupingSets = runAofM Monadic.groupingSets

-- | Same as 'Monadic.orderBy''.
--   The result arrow is designed to be injected by local projections.
orderBy' :: Monad m
         => Order
         -> Nulls
         -> Orderings c m (Projection c t) ()
orderBy' o n = queryA $ \p -> Monadic.orderBy' p o n

-- | Same as 'Monadic.orderBy'.
--   The result arrow is designed to be injected by local projections.
orderBy :: Monad m
        => Order
        -> Orderings c m (Projection c t) ()
orderBy o = queryA (`Monadic.orderBy` o)

-- | Same as 'Monadic.asc'.
--   The result arrow is designed to be injected by local projections.
asc :: Monad m
    => Orderings c m (Projection c t) ()
asc = queryA Monadic.asc

-- | Same as 'Monadic.desc'.
--   The result arrow is designed to be injected by local projections.
desc :: Monad m
     => Orderings c m (Projection c t) ()
desc = queryA Monadic.desc

-- | Same as 'Monadic.partitionBy'.
--   The result arrow is designed to be injected by local projections.
partitionBy :: Window c (Projection c r) ()
partitionBy = queryA Monadic.partitionBy

-- | Same as 'Monadic.over'.
--   Make window function result projection using built 'Window' arrow.
over :: SqlProjectable (Projection c)
     => Projection OverWindow a -> Window c () () -> Projection c a
over po = runAofM $ Monadic.over po

infix 8 `over`

-- | Make 'Monadic.AssignTarget' into arrow which is designed to be
--   injected by local projection assignees.
assign :: Monad m
       => Monadic.AssignTarget r v
       -> Assignings r m (Projection Flat v) ()
assign t = queryA (`Monadic.assignTo` t)

-- | Same as 'Monadic.derivedUpdate''.
--   Make 'Update' from assigning statement arrow using configuration.
derivedUpdate' :: TableDerivable r => Config -> AssignStatement r (PlaceHolders p) -> Update p
derivedUpdate' config = Monadic.derivedUpdate' config . runQueryA

-- | Same as 'Monadic.derivedUpdate'.
--   Make 'Update' from assigning statement arrow.
derivedUpdate :: TableDerivable r => AssignStatement r (PlaceHolders p) -> Update p
derivedUpdate = Monadic.derivedUpdate . runQueryA

-- | Same as 'Monadic.derivedInsertValue''.
--   Make 'Insert' from register arrow using configuration.
derivedInsertValue' :: TableDerivable r => Config -> Register r (PlaceHolders p) -> Insert p
derivedInsertValue' config = Monadic.derivedInsertValue' config . ($ ()) . runQueryA

-- | Same as 'Monadic.derivedInsertValue'.
--   Make 'Insert' from register arrow.
derivedInsertValue :: TableDerivable r => Register r (PlaceHolders p) -> Insert p
derivedInsertValue = Monadic.derivedInsertValue . ($ ()) . runQueryA

-- | Same as 'Monadic.derivedDelete''.
--   Make 'Update' from restrict statement arrow using configuration.
derivedDelete' :: TableDerivable r => Config -> RestrictedStatement r (PlaceHolders p) -> Delete p
derivedDelete' config = Monadic.derivedDelete' config . runQueryA

-- | Same as 'Monadic.derivedDelete'.
--   Make 'Update' from restrict statement arrow.
derivedDelete :: TableDerivable r => RestrictedStatement r (PlaceHolders p) -> Delete p
derivedDelete = Monadic.derivedDelete . runQueryA

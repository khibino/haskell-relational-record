{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Database.Relational.Arrow
-- Copyright   : 2015-2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines arrow version combinators which
-- improves type-safty on building queries.
-- Referencing the local projected records may cause to break
-- the result query.
-- It is possible to controls injection of previous local projected records
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

  update', update, updateNoPH,
  updateAllColumn', updateAllColumn, updateAllColumnNoPH,
  insertValue', insertValue, insertValueNoPH,
  delete', delete, deleteNoPH,

  QueryA,

  QuerySimple, QueryAggregate, QueryUnique,

  AggregatingSet, AggregatingSetList, AggregatingPowerSet,

  Orderings, Window, Assignings,

  AssignStatement, Register, RestrictedStatement,

  -- * Deprecated
  derivedUpdate', derivedUpdate,
  derivedInsertValue', derivedInsertValue,
  derivedDelete', derivedDelete,
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
   update', update, updateNoPH, derivedUpdate', derivedUpdate,
   updateAllColumn', updateAllColumn, updateAllColumnNoPH,
   insertValue', insertValue, insertValueNoPH, derivedInsertValue', derivedInsertValue,
   delete', delete, deleteNoPH, derivedDelete', derivedDelete,
   QuerySimple, QueryAggregate, QueryUnique, Orderings, Window, Register)
import qualified Database.Relational as Monadic
import qualified Database.Relational.Monad.Trans.Aggregating as Monadic
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
type AssignStatement r a = QueryA (Monadic.Assignings r Restrict) (Record Flat r) a

-- | Arrow type corresponding to 'Monadic.Register'
type Register r a = QueryA (Monadic.Register r) () a

-- | Arrow type corresponding to 'Monadic.RestrictedStatement'
type RestrictedStatement r a = QueryA Monadic.Restrict (Record Flat r) a


-- | Same as 'Monadic.all''. Arrow version.
all' :: MonadQuery m => QueryA m () ()
all' = queryA $ \() -> Monadic.all'

-- | Same as 'Monadic.distinct'. Arrow version.
distinct :: MonadQuery m => QueryA m () ()
distinct = queryA $ \() -> Monadic.distinct

-- | Same as 'Monadic.query'. Arrow version.
--   The result arrow is not injected by local projected records.
query :: (MonadQualify ConfigureQuery m, MonadQuery m)
      => Relation () r -> QueryA m () (Record Flat r)
query r = queryA $ \() -> Monadic.query r

-- | Same as 'Monadic.queryMaybe'. Arrow version.
--   The result arrow is not injected by any local projected records.
queryMaybe :: (MonadQualify ConfigureQuery m, MonadQuery m)
           => Relation () r -> QueryA m () (Record Flat (Maybe r))
queryMaybe r = queryA $ \() -> Monadic.queryMaybe r

-- | Same as 'Monadic.query''. Arrow version.
--   The result arrow is not injected by any local projected records.
query' :: (MonadQualify ConfigureQuery m, MonadQuery m)
       => Relation p r -> QueryA m () (PlaceHolders p, Record Flat r)
query' r = queryA $ \() -> Monadic.query' r

-- | Same as 'Monadic.queryMaybe''. Arrow version.
--   The result arrow is not injected by any local projected records.
queryMaybe' :: (MonadQualify ConfigureQuery m, MonadQuery m)
            => Relation p r -> QueryA m () (PlaceHolders p, Record Flat (Maybe r))
queryMaybe' r = queryA $ \() -> Monadic.queryMaybe' r

unsafeQueryList :: MonadQualify ConfigureQuery m
            => (a -> Relation () r)
            -> QueryA m a (RecordList (Record c) r)
unsafeQueryList rf = queryA $ Monadic.queryList . rf

unsafeQueryList' :: MonadQualify ConfigureQuery m
             => (a -> Relation p r)
             -> QueryA m a (PlaceHolders p, RecordList (Record c) r)
unsafeQueryList' rf = queryA $ Monadic.queryList' . rf

-- | Same as 'Monadic.queryList'. Arrow version.
--   The result arrow is designed to be injected by local projected records.
queryList :: MonadQualify ConfigureQuery m
          => (Record c a -> Relation () r)
          -> QueryA m (Record c a) (RecordList (Record c) r)
queryList = unsafeQueryList

-- | Same as 'Monadic.queryList''. Arrow version.
--   The result arrow is designed to be injected by local projected records.
queryList' :: MonadQualify ConfigureQuery m
           => (Record c a -> Relation p r)
           -> QueryA m (Record c a) (PlaceHolders p, RecordList (Record c) r)
queryList' = unsafeQueryList'

-- | Same as 'Monadic.queryList' to pass this result to 'exists' operator. Arrow version.
--   The result arrow is designed to be injected by local projected records.
queryExists :: MonadQualify ConfigureQuery m
          => (Record c a -> Relation () r)
          -> QueryA m (Record c a) (RecordList (Record Exists) r)
queryExists = unsafeQueryList

-- | Same as 'Monadic.queryList'' to pass this result to 'exists' operator. Arrow version.
--   The result arrow is designed to be injected by local projected records.
queryExists' :: MonadQualify ConfigureQuery m
           => (Record c a -> Relation p r)
           -> QueryA m (Record c a) (PlaceHolders p, RecordList (Record Exists) r)
queryExists' = unsafeQueryList'

-- | Same as 'Monadic.queryList'. Arrow version.
--   Useful for no reference cases to local projected records.
queryListU :: MonadQualify ConfigureQuery m
           => Relation () r
           -> QueryA m () (RecordList (Record c) r)
queryListU r = unsafeQueryList $ \() -> r

-- | Same as 'Monadic.queryList''. Arrow version.
--   Useful for no reference cases to local projected records.
queryListU' :: MonadQualify ConfigureQuery m
           => Relation p r
           -> QueryA m () (PlaceHolders p, RecordList (Record c) r)
queryListU' r = unsafeQueryList' $ \() -> r

unsafeQueryScalar :: (MonadQualify ConfigureQuery m, ScalarDegree r)
                  => (a -> UniqueRelation () c r)
                  -> QueryA m a (Record c (Maybe r))
unsafeQueryScalar rf = queryA $ Monadic.queryScalar . rf

unsafeQueryScalar' :: (MonadQualify ConfigureQuery m, ScalarDegree r)
                   => (a -> UniqueRelation p c r)
                   -> QueryA m a (PlaceHolders p, Record c (Maybe r))
unsafeQueryScalar' rf = queryA $ Monadic.queryScalar' . rf

-- | Same as 'Monadic.queryScalar'. Arrow version.
--   The result arrow is designed to be injected by any local projected record.
queryScalar :: (MonadQualify ConfigureQuery m, ScalarDegree r)
            => (Record c a -> UniqueRelation () c r)
            -> QueryA m (Record c a) (Record c (Maybe r))
queryScalar = unsafeQueryScalar

-- | Same as 'Monadic.queryScalar''. Arrow version.
--   The result arrow is designed to be injected by any local projected record.
queryScalar' :: (MonadQualify ConfigureQuery m, ScalarDegree r)
             => (Record c a -> UniqueRelation p c r)
             -> QueryA m (Record c a) (PlaceHolders p, Record c (Maybe r))
queryScalar' = unsafeQueryScalar'

-- | Same as 'Monadic.queryScalar'. Arrow version.
--   Useful for no reference cases to local projected records.
queryScalarU :: (MonadQualify ConfigureQuery m, ScalarDegree r)
            => UniqueRelation () c r
            -> QueryA m () (Record c (Maybe r))
queryScalarU r = unsafeQueryScalar $ \() -> r

-- | Same as 'Monadic.queryScalar''. Arrow version.
--   Useful for no reference cases to local projected records.
queryScalarU' :: (MonadQualify ConfigureQuery m, ScalarDegree r)
             => UniqueRelation p c r
             -> QueryA m () (PlaceHolders p, Record c (Maybe r))
queryScalarU' r = unsafeQueryScalar' $ \() -> r

-- | Same as 'Monadic.uniqueQuery''. Arrow version.
--   The result arrow is not injected by local projected records.
uniqueQuery' :: UniqueRelation p c r
             -> QueryA Monadic.QueryUnique () (PlaceHolders p, Record c r)
uniqueQuery' r = queryA $ \() -> Monadic.uniqueQuery' r

-- | Same as 'Monadic.uniqueQueryMaybe''. Arrow version.
--   The result arrow is not injected by local projected records.
uniqueQueryMaybe' :: UniqueRelation p c r
                  -> QueryA Monadic.QueryUnique () (PlaceHolders p, Record c (Maybe r))
uniqueQueryMaybe' r = queryA $ \() -> Monadic.uniqueQueryMaybe' r

-- | Same as 'Monadic.on'. Arrow version.
--   The result arrow is designed to be injected by local conditional flat-records.
on :: MonadQuery m
   => QueryA m (Predicate Flat) ()
on = queryA Monadic.on

-- | Same as 'Monadic.wheres'. Arrow version.
--   The result arrow is designed to be injected by local conditional flat-records.
wheres :: MonadRestrict Flat m
       => QueryA m (Predicate Flat) ()
wheres = queryA Monadic.wheres

-- | Same as 'Monadic.having'. Arrow version.
--   The result arrow is designed to be injected by local conditional aggregated-records.
having :: MonadRestrict Aggregated m
       => QueryA m (Predicate Aggregated) ()
having = queryA Monadic.having

-- | Same as 'Monadic.groupBy'. Arrow version.
--   The result arrow is designed to be injected by local flat-records.
groupBy :: MonadAggregate m
        => QueryA m (Record Flat r) (Record Aggregated r)
groupBy = queryA Monadic.groupBy

-- | Same as 'Monadic.placeholder'. Arrow version.
--   The result arrow is designed to be injected by locally built arrow using placeholders.
placeholder :: (PersistableWidth t, SqlContext c, Monad m)
            => QueryA m (QueryA m (Record c t) a) (PlaceHolders t, a)
placeholder = queryA $ Monadic.placeholder . runQueryA

-- | Same as 'Monadic.relation'.
--   Finalize query-building arrow instead of query-building monad.
relation :: QuerySimple () (Record Flat r)
         -> Relation () r
relation = runAofM Monadic.relation

-- | Same as 'Monadic.relation''.
--   Finalize query-building arrow instead of query-building monad.
relation' :: QuerySimple () (PlaceHolders p, Record Flat r)
          -> Relation p r
relation' = runAofM Monadic.relation'

-- | Same as 'Monadic.aggregateRelation'.
--   Finalize query-building arrow instead of query-building monad.
aggregateRelation :: QueryAggregate () (Record Aggregated r)
                  -> Relation () r
aggregateRelation = runAofM Monadic.aggregateRelation

-- | Same as 'Monadic.aggregateRelation''.
--   Finalize query-building arrow instead of query-building monad.
aggregateRelation' :: QueryAggregate () (PlaceHolders p, Record Aggregated r)
                   -> Relation p r
aggregateRelation' = runAofM Monadic.aggregateRelation'

-- | Same as 'Monadic.uniqueRelation''.
--   Finalize query-building arrow instead of query-building monad.
uniqueRelation' :: QueryUnique () (PlaceHolders p, Record c r)
                -> UniqueRelation p c r
uniqueRelation' = runAofM Monadic.uniqueRelation'

-- | Same as 'Monadic.groupBy''.
--   This arrow is designed to be injected by local 'AggregateKey'.
groupBy' :: MonadAggregate m => QueryA m (AggregateKey (Record Aggregated r)) (Record Aggregated r)
groupBy' = queryA Monadic.groupBy'

-- | Same as 'Monadic.key'.
--   This arrow is designed to be injected by local flat-records.
key :: AggregatingSet (Record Flat r) (Record Aggregated (Maybe r))
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
--   This arrow is designed to be injected by local flat-records.
bkey :: AggregatingPowerSet (Record Flat r) (Record Aggregated (Maybe r))
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
--   The result arrow is designed to be injected by local projected records.
orderBy' :: Monad m
         => Order
         -> Nulls
         -> Orderings c m (Record c t) ()
orderBy' o n = queryA $ \p -> Monadic.orderBy' p o n

-- | Same as 'Monadic.orderBy'.
--   The result arrow is designed to be injected by local projected records.
orderBy :: Monad m
        => Order
        -> Orderings c m (Record c t) ()
orderBy o = queryA (`Monadic.orderBy` o)

-- | Same as 'Monadic.asc'.
--   The result arrow is designed to be injected by local projected records.
asc :: Monad m
    => Orderings c m (Record c t) ()
asc = queryA Monadic.asc

-- | Same as 'Monadic.desc'.
--   The result arrow is designed to be injected by local projected records.
desc :: Monad m
     => Orderings c m (Record c t) ()
desc = queryA Monadic.desc

-- | Same as 'Monadic.partitionBy'.
--   The result arrow is designed to be injected by local projected records.
partitionBy :: Window c (Record c r) ()
partitionBy = queryA Monadic.partitionBy

-- | Same as 'Monadic.over'.
--   Make record of window function result using built 'Window' arrow.
over :: SqlContext c
     => Record OverWindow a -> Window c () () -> Record c a
over po = runAofM $ Monadic.over po

infix 8 `over`

-- | Make 'Monadic.AssignTarget' into arrow which is designed to be
--   injected by assignees of local projected record.
assign :: Monad m
       => Monadic.AssignTarget r v
       -> Assignings r m (Record Flat v) ()
assign t = queryA (`Monadic.assignTo` t)

-- | Same as 'Monadic.update''.
--   Make 'Update' from assigning statement arrow using configuration.
update' :: TableDerivable r => Config -> QueryA (Monadic.Assignings r Restrict) (Record Flat r) (PlaceHolders p) -> Update p
update' config = Monadic.update' config . runQueryA

-- | Same as 'Monadic.update'.
--   Make 'Update' from assigning statement arrow.
update :: TableDerivable r => QueryA (Monadic.Assignings r Restrict) (Record Flat r) (PlaceHolders p) -> Update p
update = Monadic.update . runQueryA

-- | Same as 'Monadic.updateNoPH'.
--   Make 'Update' from assigning statement arrow.
updateNoPH :: TableDerivable r => QueryA (Monadic.Assignings r Restrict) (Record Flat r) () -> Update ()
updateNoPH = Monadic.updateNoPH . runQueryA

-- | Same as 'Monadic.updateAllColumn''.
--   Make 'Update' from restrected statement arrow.
updateAllColumn' :: (PersistableWidth r, TableDerivable r)
                 => Config
                 -> QueryA Monadic.Restrict (Record Flat r) (PlaceHolders p)
                 -> Update (r, p)
updateAllColumn' config = Monadic.updateAllColumn' config . runQueryA

-- | Same as 'Monadic.updateAllColumn'.
--   Make 'Update' from restrected statement arrow.
updateAllColumn :: (PersistableWidth r, TableDerivable r)
                => QueryA Monadic.Restrict (Record Flat r) (PlaceHolders p)
                -> Update (r, p)
updateAllColumn = Monadic.updateAllColumn . runQueryA

-- | Same as 'Monadic.updateAllColumnNoPH'.
--   Make 'Update' from restrected statement arrow.
updateAllColumnNoPH :: (PersistableWidth r, TableDerivable r)
                    => QueryA Monadic.Restrict (Record Flat r) ()
                    -> Update r
updateAllColumnNoPH = Monadic.updateAllColumnNoPH . runQueryA

-- | Same as 'Monadic.insertValue''.
--   Make 'Insert' from register arrow using configuration.
insertValue' :: TableDerivable r => Config -> Register r (PlaceHolders p) -> Insert p
insertValue' config = Monadic.insertValue' config . ($ ()) . runQueryA

-- | Same as 'Monadic.insertValue'.
--   Make 'Insert' from register arrow.
insertValue :: TableDerivable r => Register r (PlaceHolders p) -> Insert p
insertValue = Monadic.insertValue . ($ ()) . runQueryA

-- | Same as 'Monadic.insertValueNoPH'.
--   Make 'Insert' from register arrow.
insertValueNoPH :: TableDerivable r => Register r () -> Insert ()
insertValueNoPH = Monadic.insertValueNoPH . ($ ()) . runQueryA


-- | Same as 'Monadic.delete''.
--   Make 'Update' from restrict statement arrow using configuration.
delete' :: TableDerivable r => Config -> QueryA Monadic.Restrict (Record Flat r) (PlaceHolders p) -> Delete p
delete' config = Monadic.delete' config . runQueryA

-- | Same as 'Monadic.delete'.
--   Make 'Update' from restrict statement arrow.
delete :: TableDerivable r => QueryA Monadic.Restrict (Record Flat r) (PlaceHolders p) -> Delete p
delete = Monadic.delete . runQueryA

-- | Same as 'Monadic.deleteNoPH'.
--   Make 'Update' from restrict statement arrow.
deleteNoPH :: TableDerivable r => QueryA Monadic.Restrict (Record Flat r) () -> Delete ()
deleteNoPH = Monadic.deleteNoPH . runQueryA

{-# DEPRECATED derivedUpdate' "use `update'` instead of this." #-}
-- | Same as 'Monadic.update''.
--   Make 'Update' from assigning statement arrow using configuration.
derivedUpdate' :: TableDerivable r => Config -> QueryA (Monadic.Assignings r Restrict) (Record Flat r) (PlaceHolders p) -> Update p
derivedUpdate' = update'

{-# DEPRECATED derivedUpdate "use `update` instead of this." #-}
-- | Deprecated.
derivedUpdate :: TableDerivable r => QueryA (Monadic.Assignings r Restrict) (Record Flat r) (PlaceHolders p) -> Update p
derivedUpdate = update

{-# DEPRECATED derivedInsertValue' "use `insertValue'` instead of this." #-}
-- | Deprecated.
derivedInsertValue' :: TableDerivable r => Config -> Register r (PlaceHolders p) -> Insert p
derivedInsertValue' = insertValue'

{-# DEPRECATED derivedInsertValue "use `insertValue` instead of this." #-}
-- | Deprecated.
derivedInsertValue :: TableDerivable r => Register r (PlaceHolders p) -> Insert p
derivedInsertValue = insertValue

{-# DEPRECATED derivedDelete' "use `derivedDelete'` instead of this." #-}
-- | Deprecated.
derivedDelete' :: TableDerivable r => Config -> QueryA Monadic.Restrict (Record Flat r) (PlaceHolders p) -> Delete p
derivedDelete' = delete'

{-# DEPRECATED derivedDelete "use `derivedDelete` instead of this." #-}
-- | Deprecated.
derivedDelete :: TableDerivable r => QueryA Monadic.Restrict (Record Flat r) (PlaceHolders p) -> Delete p
derivedDelete = delete

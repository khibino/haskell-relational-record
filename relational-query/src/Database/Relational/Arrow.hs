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

  on, wheres, having, groupBy,

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

  askPlaceholders,

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
   on, wheres, having, groupBy,
   relation, relation', aggregateRelation, aggregateRelation', uniqueRelation',
   groupBy', key, key', set, bkey, rollup, cube, groupingSets,
   orderBy', orderBy, asc, desc, partitionBy, over,
   update', update, updateNoPH, derivedUpdate', derivedUpdate,
   updateAllColumn', updateAllColumn, updateAllColumnNoPH,
   insertValue', insertValue, insertValueNoPH, derivedInsertValue', derivedInsertValue,
   delete', delete, deleteNoPH, derivedDelete', derivedDelete, askPlaceholders,
   QuerySimple, QueryAggregate, QueryUnique, Orderings, Window, Register)
import qualified Database.Relational as Monadic
import qualified Database.Relational.Monad.Trans.Aggregating as Monadic
import qualified Database.Relational.Monad.Trans.Assigning as MonadicAssigning


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
type Assignings p r m = QueryA (Monadic.ReadPlaceholders p (MonadicAssigning.Assignings r m))

-- | Arrow type corresponding to 'Monadic.AssignStatement'
type AssignStatement p r a = QueryA (Monadic.ReadPlaceholders p (MonadicAssigning.Assignings r Restrict)) (Record Flat r) a

-- | Arrow type corresponding to 'Monadic.Register'
type Register p r a = QueryA (Monadic.ReadPlaceholders p (Monadic.Register r)) () a

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
       => Relation p r -> QueryA m (Record PureOperand p) (Record Flat r)
query' r = queryA $ \phs -> Monadic.query' phs r

-- | Same as 'Monadic.queryMaybe''. Arrow version.
--   The result arrow is not injected by any local projected records.
queryMaybe' :: (MonadQualify ConfigureQuery m, MonadQuery m)
            => Relation p r -> QueryA m (Record PureOperand p) (Record Flat (Maybe r))
queryMaybe' r = queryA $ \phs -> Monadic.queryMaybe' phs r

unsafeQueryList :: MonadQualify ConfigureQuery m
                => (a -> Relation () r)
                -> QueryA m a (RecordList (Record c) r)
unsafeQueryList rf = queryA $ Monadic.queryList . rf

unsafeQueryList' :: MonadQualify ConfigureQuery m
                 => (a -> Relation p r)
                 -> QueryA m (Record PureOperand p, a) (RecordList (Record c) r)
unsafeQueryList' rf = queryA $ \(phs, x) -> Monadic.queryList' phs $ rf x

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
           -> QueryA m (Record PureOperand p, Record c a) (RecordList (Record c) r)
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
           -> QueryA m (Record PureOperand p, Record c a) (RecordList (Record Exists) r)
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
           -> QueryA m (Record PureOperand p) (RecordList (Record c) r)
queryListU' r = queryA $ \phs -> Monadic.queryList' phs r

unsafeQueryScalar :: (MonadQualify ConfigureQuery m, ScalarDegree r)
                  => (a -> UniqueRelation () c r)
                  -> QueryA m a (Record c (Maybe r))
unsafeQueryScalar rf = queryA $ Monadic.queryScalar . rf

unsafeQueryScalar' :: (MonadQualify ConfigureQuery m, ScalarDegree r)
                   => (a -> UniqueRelation p c r)
                   -> QueryA m (Record PureOperand p, a) (Record c (Maybe r))
unsafeQueryScalar' rf = queryA $ \(phs, x) ->  Monadic.queryScalar' phs $ rf x

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
             -> QueryA m (Record PureOperand p, Record c a) (Record c (Maybe r))
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
             -> QueryA m (Record PureOperand p) (Record c (Maybe r))
queryScalarU' r = queryA $ \phs -> Monadic.queryScalar' phs r

-- | Same as 'Monadic.uniqueQuery''. Arrow version.
--   The result arrow is not injected by local projected records.
uniqueQuery' :: UniqueRelation p c r
             -> QueryA Monadic.QueryUnique (Record PureOperand p) (Record c r)
uniqueQuery' r = queryA $ \phs -> Monadic.uniqueQuery' phs r

-- | Same as 'Monadic.uniqueQueryMaybe''. Arrow version.
--   The result arrow is not injected by local projected records.
uniqueQueryMaybe' :: UniqueRelation p c r
                  -> QueryA Monadic.QueryUnique (Record PureOperand p) (Record c (Maybe r))
uniqueQueryMaybe' r = queryA $ \phs -> Monadic.uniqueQueryMaybe' phs r

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

-- | Same as 'Monadic.relation'.
--   Finalize query-building arrow instead of query-building monad.
relation :: QuerySimple () (Record Flat r)
         -> Relation () r
relation = runAofM Monadic.relation

-- | Same as 'Monadic.relation''.
--   Finalize query-building arrow instead of query-building monad.
relation' :: PersistableWidth p
          => QuerySimple (Record PureOperand p) (Record Flat r)
          -> Relation p r
relation' = Monadic.relation' . runQueryA

-- | Same as 'Monadic.aggregateRelation'.
--   Finalize query-building arrow instead of query-building monad.
aggregateRelation :: QueryAggregate () (Record Aggregated r)
                  -> Relation () r
aggregateRelation = runAofM Monadic.aggregateRelation

-- | Same as 'Monadic.aggregateRelation''.
--   Finalize query-building arrow instead of query-building monad.
aggregateRelation' :: PersistableWidth p
                   => QueryAggregate (Record PureOperand p) (Record Aggregated r)
                   -> Relation p r
aggregateRelation' = Monadic.aggregateRelation' . runQueryA

-- | Same as 'Monadic.uniqueRelation''.
--   Finalize query-building arrow instead of query-building monad.
uniqueRelation' :: PersistableWidth p
                => QueryUnique (Record PureOperand p) (Record c r)
                -> UniqueRelation p c r
uniqueRelation' = Monadic.uniqueRelation' . runQueryA

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
       => MonadicAssigning.AssignTarget r v
       -> Assignings p r m (Record Flat v) ()
assign t = queryA (`Monadic.assignTo` t)

-- | Same as 'Monadic.update''.
--   Make 'Update' from assigning statement arrow using configuration.
update' :: (PersistableWidth p, TableDerivable r) => Config -> QueryA (Monadic.ReadPlaceholders p (MonadicAssigning.Assignings r Restrict)) (Record Flat r) () -> Update p
update' config = Monadic.update' config . runQueryA

-- | Same as 'Monadic.update'.
--   Make 'Update' from assigning statement arrow.
update :: (PersistableWidth p, TableDerivable r) => QueryA (Monadic.ReadPlaceholders p (MonadicAssigning.Assignings r Restrict)) (Record Flat r) () -> Update p
update = Monadic.update . runQueryA

-- | Same as 'Monadic.updateNoPH'.
--   Make 'Update' from assigning statement arrow.
updateNoPH :: TableDerivable r => QueryA (Monadic.ReadPlaceholders () (MonadicAssigning.Assignings r Restrict)) (Record Flat r) () -> Update ()
updateNoPH = Monadic.updateNoPH . runQueryA

-- | Same as 'Monadic.updateAllColumn''.
--   Make 'Update' from restrected statement arrow.
updateAllColumn' :: (PersistableWidth p, PersistableWidth r, TableDerivable r)
                 => Config
                 -> QueryA (ReadPlaceholders p Monadic.Restrict) (Record Flat r) ()
                 -> Update (r, p)
updateAllColumn' config = Monadic.updateAllColumn' config . runQueryA

-- | Same as 'Monadic.updateAllColumn'.
--   Make 'Update' from restrected statement arrow.
updateAllColumn :: (PersistableWidth p, PersistableWidth r, TableDerivable r)
                => QueryA (ReadPlaceholders p Monadic.Restrict) (Record Flat r) ()
                -> Update (r, p)
updateAllColumn = Monadic.updateAllColumn . runQueryA

-- | Same as 'Monadic.updateAllColumnNoPH'.
--   Make 'Update' from restrected statement arrow.
updateAllColumnNoPH :: (PersistableWidth r, TableDerivable r)
                    => QueryA (Monadic.ReadPlaceholders () Monadic.Restrict) (Record Flat r) ()
                    -> Update r
updateAllColumnNoPH = Monadic.updateAllColumnNoPH . runQueryA

-- | Same as 'Monadic.insertValue''.
--   Make 'Insert' from register arrow using configuration.
insertValue' :: (PersistableWidth p, TableDerivable r) => Config -> Register p r () -> Insert p
insertValue' config = Monadic.insertValue' config . ($ ()) . runQueryA

-- | Same as 'Monadic.insertValue'.
--   Make 'Insert' from register arrow.
insertValue :: (PersistableWidth p, TableDerivable r) => Register p r () -> Insert p
insertValue = Monadic.insertValue . ($ ()) . runQueryA

-- | Same as 'Monadic.insertValueNoPH'.
--   Make 'Insert' from register arrow.
insertValueNoPH :: TableDerivable r => Register () r () -> Insert ()
insertValueNoPH = Monadic.insertValueNoPH . ($ ()) . runQueryA


-- | Same as 'Monadic.delete''.
--   Make 'Update' from restrict statement arrow using configuration.
delete' :: (PersistableWidth p, TableDerivable r) => Config -> QueryA (ReadPlaceholders p Monadic.Restrict) (Record Flat r) () -> Delete p
delete' config = Monadic.delete' config . runQueryA

-- | Same as 'Monadic.delete'.
--   Make 'Update' from restrict statement arrow.
delete :: (PersistableWidth p, TableDerivable r) => QueryA (ReadPlaceholders p Monadic.Restrict) (Record Flat r) () -> Delete p
delete = Monadic.delete . runQueryA

-- | Same as 'Monadic.askPlaceholders'.
askPlaceholders :: Monad m => QueryA (ReadPlaceholders p m) () (Record PureOperand p)
askPlaceholders = queryA $ \() -> Monadic.askPlaceholders

-- | Same as 'Monadic.deleteNoPH'.
--   Make 'Update' from restrict statement arrow.
deleteNoPH :: TableDerivable r => QueryA (ReadPlaceholders () Monadic.Restrict) (Record Flat r) () -> Delete ()
deleteNoPH = Monadic.deleteNoPH . runQueryA

{-# DEPRECATED derivedUpdate' "use `update'` instead of this." #-}
-- | Same as 'Monadic.update''.
--   Make 'Update' from assigning statement arrow using configuration.
derivedUpdate' :: (PersistableWidth p, TableDerivable r) => Config -> QueryA (Monadic.ReadPlaceholders p (MonadicAssigning.Assignings r Restrict)) (Record Flat r) () -> Update p
derivedUpdate' = update'

{-# DEPRECATED derivedUpdate "use `update` instead of this." #-}
-- | Deprecated.
derivedUpdate :: (PersistableWidth p, TableDerivable r) => QueryA (Monadic.ReadPlaceholders p (MonadicAssigning.Assignings r Restrict)) (Record Flat r) () -> Update p
derivedUpdate = update

{-# DEPRECATED derivedInsertValue' "use `insertValue'` instead of this." #-}
-- | Deprecated.
derivedInsertValue' :: (PersistableWidth p, TableDerivable r) => Config -> Register p r () -> Insert p
derivedInsertValue' = insertValue'

{-# DEPRECATED derivedInsertValue "use `insertValue` instead of this." #-}
-- | Deprecated.
derivedInsertValue :: (PersistableWidth p, TableDerivable r) => Register p r () -> Insert p
derivedInsertValue = insertValue

{-# DEPRECATED derivedDelete' "use `derivedDelete'` instead of this." #-}
-- | Deprecated.
derivedDelete' :: (PersistableWidth p, TableDerivable r) => Config -> QueryA (Monadic.ReadPlaceholders p Monadic.Restrict) (Record Flat r) () -> Delete p
derivedDelete' = delete'

{-# DEPRECATED derivedDelete "use `derivedDelete` instead of this." #-}
-- | Deprecated.
derivedDelete :: (PersistableWidth p, TableDerivable r) => QueryA (Monadic.ReadPlaceholders p Monadic.Restrict) (Record Flat r) () -> Delete p
derivedDelete = delete

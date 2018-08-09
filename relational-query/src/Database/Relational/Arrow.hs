{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Database.Relational.Arrow
-- Copyright   : 2015-2018 Kei Hibino
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
   QuerySimple, QueryAggregate, QueryUnique, Window, Register)
import qualified Database.Relational as Monadic
import Database.Relational.ReboundSyntax (IxMonad)
import Database.Relational.ExtensibleRecord (ExRecord, type (++))
import qualified Database.Relational.Monad.Trans.Aggregating as Monadic
import qualified Database.Relational.Monad.Trans.Ordering as Monadic
import qualified Database.Relational.Monad.Trans.Assigning as Monadic
import qualified Database.Relational.Monad.Trans.Placeholders as Monadic


-- | Arrow to build queries.
newtype QueryA m a b = QueryA (Kleisli m a b) deriving (Category, Arrow)

queryA :: (a -> m b) -> QueryA m a b
queryA = QueryA . Kleisli

runQueryA :: QueryA m a b -> a -> m b
runQueryA (QueryA k) = runKleisli k

runAofM :: (m b -> c) -> QueryA m () b -> c
runAofM = (. (`runQueryA` ()))

-- | Arrow type corresponding to 'Monadic.QuerySimple'
type QuerySimple i j = QueryA (Monadic.QuerySimple i j)

-- | Arrow type corresponding to 'Monadic.QueryAggregate'
type QueryAggregate i j = QueryA (Monadic.QueryAggregate i j)

-- | Arrow type corresponding to 'Monadic.QueryUnique'
type QueryUnique i j = QueryA (Monadic.QueryUnique i j)

-- | Arrow type corresponding to 'Monadic.AggregatingSet'
type AggregatingSet i j = QueryA (Monadic.AggregatingSet i j)

-- | Arrow type corresponding to 'Monadic.AggregatingSetList'
type AggregatingSetList i j = QueryA (Monadic.AggregatingSetList i j)

-- | Arrow type corresponding to 'Monadic.AggregatingPowerSet'
type AggregatingPowerSet i j = QueryA (Monadic.AggregatingPowerSet i j)

-- | Arrow type corresponding to 'Monadic.Orderings'
type Orderings c m i j = QueryA (Monadic.Placeholders (Monadic.Orderings c m) i j)

-- | Arrow type corresponding to 'Monadic.Window'
type Window c i j = QueryA (Monadic.Window c i j)

-- | Arrow type corresponding to 'Monadic.Assignings'
type Assignings r m i j = QueryA (Monadic.Placeholders (Monadic.Assignings r m) i j)

-- | Arrow type corresponding to 'Monadic.AssignStatement'
type AssignStatement r i j a = Assignings r Monadic.Restrict i j (Record (ExRecord '[]) (ExRecord '[]) Flat r) a

-- | Arrow type corresponding to 'Monadic.Register'
type Register i j r a = QueryA (Monadic.Register i j r) () a

-- | Arrow type corresponding to 'Monadic.RestrictedStatement'
type RestrictedStatement i j r a = QueryA (Monadic.Placeholders Monadic.Restrict i j) (Record i j Flat r) a


-- | Same as 'Monadic.all''. Arrow version.
all' :: MonadQuery (m i j) => QueryA (m i j) () ()
all' = queryA $ \() -> Monadic.all'

-- | Same as 'Monadic.distinct'. Arrow version.
distinct :: MonadQuery (m i j) => QueryA (m i j) () ()
distinct = queryA $ \() -> Monadic.distinct

-- | Same as 'Monadic.query'. Arrow version.
--   The result arrow is not injected by local projected records.
query :: (MonadQualify ConfigureQuery m, MonadQuery m)
      => Relation (ExRecord ys) (ExRecord zs) () r -> QueryA (Monadic.Placeholders m (ExRecord xs) (ExRecord (xs ++ zs))) () (Record (ExRecord '[]) (ExRecord '[]) Flat r)
query r = queryA $ \() -> Monadic.query r

-- | Same as 'Monadic.queryMaybe'. Arrow version.
--   The result arrow is not injected by any local projected records.
queryMaybe :: (MonadQualify ConfigureQuery m, MonadQuery m)
           => Relation (ExRecord ys) (ExRecord zs) () r -> QueryA (Monadic.Placeholders m (ExRecord xs) (ExRecord (xs ++ zs))) () (Record (ExRecord '[]) (ExRecord '[]) Flat (Maybe r))
queryMaybe r = queryA $ \() -> Monadic.queryMaybe r

-- | Same as 'Monadic.query''. Arrow version.
--   The result arrow is not injected by any local projected records.
query' :: (MonadQualify ConfigureQuery m, MonadQuery m)
       => Relation (ExRecord ys) (ExRecord zs) p r -> QueryA (Monadic.Placeholders m (ExRecord xs) (ExRecord (xs ++ zs))) () (PlaceHolders p, Record (ExRecord '[]) (ExRecord '[]) Flat r)
query' r = queryA $ \() -> Monadic.query' r

-- | Same as 'Monadic.queryMaybe''. Arrow version.
--   The result arrow is not injected by any local projected records.
queryMaybe' :: (MonadQualify ConfigureQuery m, MonadQuery m)
            => Relation (ExRecord ys) (ExRecord zs) p r -> QueryA (Monadic.Placeholders m (ExRecord xs) (ExRecord (xs ++ zs))) () (PlaceHolders p, Record (ExRecord '[]) (ExRecord '[]) Flat (Maybe r))
queryMaybe' r = queryA $ \() -> Monadic.queryMaybe' r

unsafeQueryList :: MonadQualify ConfigureQuery m
            => (a -> Relation (ExRecord ys) (ExRecord zs) () r)
            -> QueryA (Monadic.Placeholders m (ExRecord xs) (ExRecord (xs ++ zs))) a (RecordList (Record (ExRecord '[]) (ExRecord '[]) c) r)
unsafeQueryList rf = queryA $ Monadic.queryList . rf

unsafeQueryList' :: MonadQualify ConfigureQuery m
             => (a -> Relation (ExRecord ys) (ExRecord zs) p r)
             -> QueryA (Monadic.Placeholders m (ExRecord xs) (ExRecord (xs ++ zs))) a (PlaceHolders p, RecordList (Record (ExRecord '[]) (ExRecord '[]) c) r)
unsafeQueryList' rf = queryA $ Monadic.queryList' . rf

-- | Same as 'Monadic.queryList'. Arrow version.
--   The result arrow is designed to be injected by local projected records.
queryList :: MonadQualify ConfigureQuery m
          => (Record (ExRecord ys) (ExRecord zs) c a -> Relation (ExRecord ys) (ExRecord zs) () r)
          -> QueryA (Monadic.Placeholders m (ExRecord xs) (ExRecord (xs ++ zs))) (Record (ExRecord ys) (ExRecord zs) c a) (RecordList (Record (ExRecord '[]) (ExRecord '[]) c) r)
queryList = unsafeQueryList

-- | Same as 'Monadic.queryList''. Arrow version.
--   The result arrow is designed to be injected by local projected records.
queryList' :: MonadQualify ConfigureQuery m
           => (Record (ExRecord ys) (ExRecord zs) c a -> Relation (ExRecord ys) (ExRecord zs) p r)
           -> QueryA (Monadic.Placeholders m (ExRecord xs) (ExRecord (xs ++ zs))) (Record (ExRecord ys) (ExRecord zs) c a) (PlaceHolders p, RecordList (Record (ExRecord '[]) (ExRecord '[]) c) r)
queryList' = unsafeQueryList'

-- | Same as 'Monadic.queryList' to pass this result to 'exists' operator. Arrow version.
--   The result arrow is designed to be injected by local projected records.
queryExists :: MonadQualify ConfigureQuery m
            => (Record (ExRecord ys) (ExRecord zs) c a -> Relation (ExRecord ys) (ExRecord zs) () r)
            -> QueryA (Monadic.Placeholders m (ExRecord xs) (ExRecord (xs ++ zs))) (Record (ExRecord ys) (ExRecord zs) c a) (RecordList (Record (ExRecord '[]) (ExRecord '[]) Exists) r)
queryExists = unsafeQueryList

-- | Same as 'Monadic.queryList'' to pass this result to 'exists' operator. Arrow version.
--   The result arrow is designed to be injected by local projected records.
queryExists' :: MonadQualify ConfigureQuery m
           => (Record (ExRecord ys) (ExRecord zs) c a -> Relation (ExRecord ys) (ExRecord zs) p r)
           -> QueryA (Monadic.Placeholders m (ExRecord xs) (ExRecord (xs ++ zs))) (Record (ExRecord ys) (ExRecord zs) c a) (PlaceHolders p, RecordList (Record (ExRecord '[]) (ExRecord '[]) Exists) r)
queryExists' = unsafeQueryList'

-- | Same as 'Monadic.queryList'. Arrow version.
--   Useful for no reference cases to local projected records.
queryListU :: MonadQualify ConfigureQuery m
           => Relation (ExRecord ys) (ExRecord zs) () r
           -> QueryA (Monadic.Placeholders m (ExRecord xs) (ExRecord (xs ++ zs))) () (RecordList (Record (ExRecord '[]) (ExRecord '[]) c) r)
queryListU r = unsafeQueryList $ \() -> r

-- | Same as 'Monadic.queryList''. Arrow version.
--   Useful for no reference cases to local projected records.
queryListU' :: MonadQualify ConfigureQuery m
           => Relation (ExRecord ys) (ExRecord zs) p r
           -> QueryA (Monadic.Placeholders m (ExRecord xs) (ExRecord (xs ++ zs))) () (PlaceHolders p, RecordList (Record (ExRecord '[]) (ExRecord '[]) c) r)
queryListU' r = unsafeQueryList' $ \() -> r

unsafeQueryScalar :: (MonadQualify ConfigureQuery m, ScalarDegree r)
                  => (a -> UniqueRelation (ExRecord xs) (ExRecord ys) () c r)
                  -> QueryA
                      (Monadic.Placeholders m (ExRecord xs) (ExRecord ys))
                      a
                      (Record (ExRecord '[]) (ExRecord ys) c (Maybe r))
unsafeQueryScalar rf = queryA $ Monadic.queryScalar . rf

unsafeQueryScalar' :: (MonadQualify ConfigureQuery m, ScalarDegree r)
                   => (a -> UniqueRelation (ExRecord xs) (ExRecord ys) p c r)
                   -> QueryA
                        (Monadic.Placeholders m (ExRecord xs) (ExRecord ys))
                        a
                        (PlaceHolders p, Record (ExRecord '[]) (ExRecord ys) c (Maybe r))
unsafeQueryScalar' rf = queryA $ Monadic.queryScalar' . rf

-- | Same as 'Monadic.queryScalar'. Arrow version.
--   The result arrow is designed to be injected by any local projected record.
queryScalar :: (MonadQualify ConfigureQuery m, ScalarDegree r)
            => (Record (ExRecord '[]) (ExRecord ys) c a -> UniqueRelation (ExRecord xs) (ExRecord ys) () c r)
            -> QueryA
                (Monadic.Placeholders m (ExRecord xs) (ExRecord ys))
                (Record (ExRecord '[]) (ExRecord ys) c a)
                (Record (ExRecord '[]) (ExRecord ys) c (Maybe r))
queryScalar = unsafeQueryScalar

-- | Same as 'Monadic.queryScalar''. Arrow version.
--   The result arrow is designed to be injected by any local projected record.
queryScalar' :: (MonadQualify ConfigureQuery m, ScalarDegree r)
             => (Record (ExRecord '[]) (ExRecord ys) c a -> UniqueRelation (ExRecord xs) (ExRecord ys) p c r)
             -> QueryA
                  (Monadic.Placeholders m (ExRecord xs) (ExRecord ys))
                  (Record (ExRecord '[]) (ExRecord ys) c a)
                  (PlaceHolders p, Record (ExRecord '[]) (ExRecord ys) c (Maybe r))
queryScalar' = unsafeQueryScalar'

-- | Same as 'Monadic.queryScalar'. Arrow version.
--   Useful for no reference cases to local projected records.
queryScalarU :: (MonadQualify ConfigureQuery m, ScalarDegree r)
            => UniqueRelation (ExRecord xs) (ExRecord ys) () c r
            -> QueryA
                 (Monadic.Placeholders m (ExRecord xs) (ExRecord ys))
                 ()
                 (Record (ExRecord '[])
                 (ExRecord ys) c (Maybe r))
queryScalarU r = unsafeQueryScalar $ \() -> r

-- | Same as 'Monadic.queryScalar''. Arrow version.
--   Useful for no reference cases to local projected records.
queryScalarU' :: (MonadQualify ConfigureQuery m, ScalarDegree r)
             => UniqueRelation (ExRecord xs) (ExRecord ys) p c r
             -> QueryA
                 (Monadic.Placeholders m (ExRecord xs) (ExRecord ys))
                 ()
                 (PlaceHolders p, Record (ExRecord '[])
                 (ExRecord ys) c (Maybe r))
queryScalarU' r = unsafeQueryScalar' $ \() -> r

-- | Same as 'Monadic.uniqueQuery''. Arrow version.
--   The result arrow is not injected by local projected records.
uniqueQuery' :: UniqueRelation (ExRecord '[]) (ExRecord ys) p c r
             -> QueryA (Monadic.QueryUnique (ExRecord xs) (ExRecord (xs ++ ys))) () (PlaceHolders p, Record (ExRecord '[]) (ExRecord ys) c r)
uniqueQuery' r = queryA $ \() -> Monadic.uniqueQuery' r

-- | Same as 'Monadic.uniqueQueryMaybe''. Arrow version.
--   The result arrow is not injected by local projected records.
uniqueQueryMaybe' :: UniqueRelation (ExRecord '[]) (ExRecord ys) p c r
                  -> QueryA (Monadic.QueryUnique (ExRecord xs) (ExRecord (xs ++ ys))) () (PlaceHolders p, Record (ExRecord '[]) (ExRecord ys) c (Maybe r))
uniqueQueryMaybe' r = queryA $ \() -> Monadic.uniqueQueryMaybe' r

-- | Same as 'Monadic.on'. Arrow version.
--   The result arrow is designed to be injected by local conditional flat-records.
on :: (MonadQuery m)
   => QueryA (Monadic.Placeholders m (ExRecord xs) (ExRecord (xs ++ ys))) (Predicate (ExRecord '[]) (ExRecord ys) Flat) ()
on = queryA Monadic.on

-- | Same as 'Monadic.wheres'. Arrow version.
--   The result arrow is designed to be injected by local conditional flat-records.
wheres :: (MonadRestrict Flat m)
       => QueryA (Monadic.Placeholders m (ExRecord xs) (ExRecord (xs ++ ys))) (Predicate (ExRecord '[]) (ExRecord ys) Flat) ()
wheres = queryA Monadic.wheres

-- | Same as 'Monadic.having'. Arrow version.
--   The result arrow is designed to be injected by local conditional aggregated-records.
having :: (MonadRestrict Aggregated m)
       => QueryA (Monadic.Placeholders m (ExRecord xs) (ExRecord (xs ++ ys))) (Predicate (ExRecord '[]) (ExRecord ys) Aggregated) ()
having = queryA Monadic.having

-- | Same as 'Monadic.groupBy'. Arrow version.
--   The result arrow is designed to be injected by local flat-records.
groupBy :: MonadAggregate m
        => QueryA (Monadic.Placeholders m (ExRecord xs) (ExRecord (xs ++ ys))) (Record (ExRecord '[]) (ExRecord ys) Flat r) (Record (ExRecord '[]) (ExRecord '[]) Aggregated r)
groupBy = queryA Monadic.groupBy

-- | Same as 'Monadic.placeholder'. Arrow version.
--   The result arrow is designed to be injected by locally built arrow using placeholders.
placeholder :: (PersistableWidth t, SqlContext c, IxMonad m)
            => QueryA (m i j) (QueryA (m i j) (Record (ExRecord '[]) j c t) a) (PlaceHolders t, a)
placeholder = queryA $ Monadic.placeholder . runQueryA

-- | Same as 'Monadic.relation'.
--   Finalize query-building arrow instead of query-building monad.
relation :: QuerySimple i j () (Record (ExRecord '[]) (ExRecord '[]) Flat r)
         -> Relation i j () r
relation = runAofM Monadic.relation

-- | Same as 'Monadic.relation''.
--   Finalize query-building arrow instead of query-building monad.
relation' :: QuerySimple i j () (PlaceHolders p, Record (ExRecord '[]) (ExRecord '[]) Flat r)
          -> Relation i j p r
relation' = runAofM Monadic.relation'

-- | Same as 'Monadic.aggregateRelation'.
--   Finalize query-building arrow instead of query-building monad.
aggregateRelation :: QueryAggregate i j () (Record (ExRecord '[]) (ExRecord '[]) Aggregated r)
                  -> Relation i j () r
aggregateRelation = runAofM Monadic.aggregateRelation

-- | Same as 'Monadic.aggregateRelation''.
--   Finalize query-building arrow instead of query-building monad.
aggregateRelation' :: QueryAggregate i j () (PlaceHolders p, Record (ExRecord '[]) (ExRecord '[]) Aggregated r)
                   -> Relation i j p r
aggregateRelation' = runAofM Monadic.aggregateRelation'

-- | Same as 'Monadic.uniqueRelation''.
--   Finalize query-building arrow instead of query-building monad.
uniqueRelation' :: QueryUnique (ExRecord '[]) j () (PlaceHolders p, Record (ExRecord '[]) j c r)
                -> UniqueRelation (ExRecord '[]) j p c r
uniqueRelation' = runAofM Monadic.uniqueRelation'

-- | Same as 'Monadic.groupBy''.
--   This arrow is designed to be injected by local 'AggregateKey'.
groupBy' :: MonadAggregate m
         => QueryA (Monadic.Placeholders m (ExRecord xs) (ExRecord (xs ++ ys))) (AggregateKey (Record (ExRecord '[]) (ExRecord ys) Aggregated r)) (Record (ExRecord '[]) (ExRecord '[]) Aggregated r)
groupBy' = queryA Monadic.groupBy'

-- | Same as 'Monadic.key'.
--   This arrow is designed to be injected by local flat-records.
key :: AggregatingSet (ExRecord '[]) (ExRecord '[]) (Record (ExRecord '[]) j Flat r) (Record (ExRecord '[]) j Aggregated (Maybe r))
key = queryA Monadic.key

-- | Same as 'Monadic.key''.
--   This arrow is designed to be injected by local 'AggregteKey'.
key' :: AggregatingSet i i (AggregateKey a) a
key' = queryA Monadic.key'

-- | Same as 'Monadic.set'.
--   This arrow is designed to be injected by locally built 'AggregtingSet' arrow.
set :: AggregatingSetList i i (AggregatingSet i i () a) a
set = queryA $ runAofM Monadic.set

-- | Same as 'Monadic.bkey'.
--   This arrow is designed to be injected by local flat-records.
bkey :: AggregatingPowerSet (ExRecord '[]) (ExRecord '[]) (Record (ExRecord '[]) j Flat r) (Record (ExRecord '[]) j Aggregated (Maybe r))
bkey = queryA Monadic.bkey

-- | Same as 'Monadic.rollup'.
--   Finalize locally built 'AggregatingPowerSet'.
rollup :: AggregatingPowerSet i j () a -> AggregateKey a
rollup = runAofM Monadic.rollup

-- | Same as 'Monadic.cube'.
--   Finalize locally built 'AggregatingPowerSet'.
cube :: AggregatingPowerSet i j () a -> AggregateKey a
cube = runAofM Monadic.cube

-- | Same as 'Monadic.groupingSets'.
--   Finalize locally built 'AggregatingSetList'.
groupingSets :: AggregatingSetList i j () a -> AggregateKey a
groupingSets = runAofM Monadic.groupingSets

-- | Same as 'Monadic.orderBy''.
--   The result arrow is designed to be injected by local projected records.
orderBy' :: Monad m
         => Order
         -> Nulls
         -> Orderings c m (ExRecord xs) (ExRecord (xs ++ ys)) (Record (ExRecord '[]) (ExRecord ys) c t) ()
orderBy' o n = queryA (\p -> Monadic.orderBy' p o n)

-- | Same as 'Monadic.orderBy'.
--   The result arrow is designed to be injected by local projected records.
orderBy :: Monad m
        => Order
        -> Orderings c m (ExRecord xs) (ExRecord (xs ++ ys)) (Record (ExRecord '[]) (ExRecord ys) c t) ()
orderBy o = queryA (`Monadic.orderBy` o)

-- | Same as 'Monadic.asc'.
--   The result arrow is designed to be injected by local projected records.
asc :: Monad m
    => Orderings c m (ExRecord xs) (ExRecord (xs ++ ys)) (Record (ExRecord '[]) (ExRecord ys) c t) ()
asc = queryA Monadic.asc

-- | Same as 'Monadic.desc'.
--   The result arrow is designed to be injected by local projected records.
desc :: Monad m
     => Orderings c m (ExRecord xs) (ExRecord (xs ++ ys)) (Record (ExRecord '[]) (ExRecord ys) c t) ()
desc = queryA Monadic.desc

-- | Same as 'Monadic.partitionBy'.
--   The result arrow is designed to be injected by local projected records.
partitionBy :: Window c (ExRecord xs) (ExRecord (xs ++ ys)) (Record (ExRecord '[]) (ExRecord ys) c r) ()
partitionBy = queryA Monadic.partitionBy

-- | Same as 'Monadic.over'.
--   Make record of window function result using built 'Window' arrow.
over :: SqlContext c
     => Record (ExRecord '[]) j OverWindow a -> Window c (ExRecord '[]) j () () -> Record (ExRecord '[]) j c a
over po = runAofM (Monadic.over po)

infix 8 `over`

-- | Make 'Monadic.AssignTarget' into arrow which is designed to be
--   injected by assignees of local projected record.
assign :: Monad m
       => Monadic.AssignTarget r v
       -> Assignings r m (ExRecord xs) (ExRecord (xs ++ ys)) (Record (ExRecord '[]) (ExRecord ys) Flat v) ()
assign t = queryA (`Monadic.assignTo` t)

-- | Same as 'Monadic.update''.
--   Make 'Update' from assigning statement arrow using configuration.
update' :: TableDerivable r => Config -> AssignStatement r (ExRecord '[]) j (PlaceHolders p) -> Update p
update' config = Monadic.update' config . runQueryA

-- | Same as 'Monadic.update'.
--   Make 'Update' from assigning statement arrow.
update :: TableDerivable r => AssignStatement r (ExRecord '[]) j (PlaceHolders p) -> Update p
update = Monadic.update . runQueryA

-- | Same as 'Monadic.updateNoPH'.
--   Make 'Update' from assigning statement arrow.
updateNoPH :: TableDerivable r => AssignStatement r (ExRecord '[]) (ExRecord '[]) () -> Update ()
updateNoPH = Monadic.updateNoPH . runQueryA

-- | Same as 'Monadic.updateAllColumn''.
--   Make 'Update' from restrected statement arrow.
updateAllColumn' :: (PersistableWidth r, TableDerivable r)
                 => Config
                 -> RestrictedStatement (ExRecord '[]) (ExRecord xs) r (PlaceHolders p)
                 -> Update (r, p)
updateAllColumn' config rs = Monadic.updateAllColumn' config (Monadic.extractPlaceholders . runQueryA rs)

-- | Same as 'Monadic.updateAllColumn'.
--   Make 'Update' from restrected statement arrow.
updateAllColumn :: (PersistableWidth r, TableDerivable r)
                => RestrictedStatement (ExRecord '[]) (ExRecord xs) r (PlaceHolders p)
                -> Update (r, p)
updateAllColumn rs = Monadic.updateAllColumn (Monadic.extractPlaceholders . runQueryA rs)

-- | Same as 'Monadic.updateAllColumnNoPH'.
--   Make 'Update' from restrected statement arrow.
-- igrep TODO: unused?
updateAllColumnNoPH :: (PersistableWidth r, TableDerivable r)
                    => RestrictedStatement (ExRecord '[]) (ExRecord xs) r ()
                    -> Update r
updateAllColumnNoPH rs = Monadic.updateAllColumnNoPH (Monadic.extractPlaceholders . runQueryA rs)

-- | Same as 'Monadic.insertValue''.
--   Make 'Insert' from register arrow using configuration.
insertValue' :: TableDerivable r => Config -> Register i j r (PlaceHolders p) -> Insert p
insertValue' config rs = Monadic.insertValue' config . ($ ()) $ runQueryA rs

-- | Same as 'Monadic.insertValue'.
--   Make 'Insert' from register arrow.
insertValue :: TableDerivable r => Register i j r (PlaceHolders p) -> Insert p
insertValue rs = Monadic.insertValue . ($ ()) $ runQueryA rs

-- | Same as 'Monadic.insertValueNoPH'.
--   Make 'Insert' from register arrow.
insertValueNoPH :: TableDerivable r => Register i i r () -> Insert ()
insertValueNoPH rs = Monadic.insertValueNoPH . ($ ()) $ runQueryA rs


-- | Same as 'Monadic.delete''.
--   Make 'Update' from restrict statement arrow using configuration.
delete' :: TableDerivable r => Config -> RestrictedStatement (ExRecord '[]) (ExRecord xs) r (PlaceHolders p) -> Delete p
delete' config rs = Monadic.delete' config (Monadic.extractPlaceholders . runQueryA rs)

-- | Same as 'Monadic.delete'.
--   Make 'Update' from restrict statement arrow.
delete :: TableDerivable r => RestrictedStatement (ExRecord '[]) (ExRecord xs) r (PlaceHolders p) -> Delete p
delete rs = Monadic.delete (Monadic.extractPlaceholders . runQueryA rs)

-- | Same as 'Monadic.deleteNoPH'.
--   Make 'Update' from restrict statement arrow.
deleteNoPH :: TableDerivable r => RestrictedStatement (ExRecord '[]) (ExRecord xs) r () -> Delete ()
deleteNoPH rs = Monadic.deleteNoPH (Monadic.extractPlaceholders . runQueryA rs)

{-# DEPRECATED derivedUpdate' "use `update'` instead of this." #-}
-- | Same as 'Monadic.update''.
--   Make 'Update' from assigning statement arrow using configuration.
derivedUpdate' :: TableDerivable r => Config -> AssignStatement r (ExRecord '[]) j (PlaceHolders p) -> Update p
derivedUpdate' = update'

{-# DEPRECATED derivedUpdate "use `update` instead of this." #-}
-- | Deprecated.
derivedUpdate :: TableDerivable r => AssignStatement r (ExRecord '[]) j (PlaceHolders p) -> Update p
derivedUpdate = update

{-# DEPRECATED derivedInsertValue' "use `insertValue'` instead of this." #-}
-- | Deprecated.
derivedInsertValue' :: TableDerivable r => Config -> Register i j r (PlaceHolders p) -> Insert p
derivedInsertValue' = insertValue'

{-# DEPRECATED derivedInsertValue "use `insertValue` instead of this." #-}
-- | Deprecated.
derivedInsertValue :: TableDerivable r => Register i j r (PlaceHolders p) -> Insert p
derivedInsertValue = insertValue

{-# DEPRECATED derivedDelete' "use `derivedDelete'` instead of this." #-}
-- | Deprecated.
derivedDelete' :: TableDerivable r => Config -> RestrictedStatement (ExRecord '[]) (ExRecord xs) r (PlaceHolders p) -> Delete p
derivedDelete' = delete'

{-# DEPRECATED derivedDelete "use `derivedDelete` instead of this." #-}
-- | Deprecated.
derivedDelete :: TableDerivable r => RestrictedStatement (ExRecord '[]) (ExRecord xs) r (PlaceHolders p) -> Delete p
derivedDelete = delete

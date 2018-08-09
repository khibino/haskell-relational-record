{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Database.Relational.Relation
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module integrate monadic operations to compose complex queries
-- with re-usable Relation type.
module Database.Relational.Relation (
  -- * Relation type
  table, derivedRelation, tableOf,
  relation, relation',
  aggregateRelation, aggregateRelation',

  UniqueRelation,
  unsafeUnique, unUnique,

  uniqueRelation', aggregatedUnique,

  -- * Query using relation
  query, queryMaybe, queryList, queryList', queryScalar, queryScalar',
  uniqueQuery', uniqueQueryMaybe',
  ) where

import Control.Applicative ((<$>))
import Database.Relational.ReboundSyntax (ireturn, IxMonad)
import qualified Database.Relational.ReboundSyntax as ReboundSyntax

import Database.Relational.ExtensibleRecord
import Database.Relational.Internal.ContextType (Flat, Aggregated)
import Database.Relational.SqlSyntax (NodeAttr(Just', Maybe), Record, Qualified, SubQuery)

import Database.Relational.Monad.BaseType
  (ConfigureQuery, qualifyQuery,
   Relation, unsafeTypeRelation, untypeRelation, relationWidth)
import Database.Relational.Monad.Class (MonadQualify (liftQualify), MonadQuery)
import Database.Relational.Monad.Trans.Placeholders
  (Placeholders, query', queryMaybe', addPlaceholders, addingPlaceholders, forceSettingPlaceholders)
import Database.Relational.Monad.Simple (QuerySimple, SimpleQuery)
import qualified Database.Relational.Monad.Simple as Simple
import Database.Relational.Monad.Aggregate (QueryAggregate, AggregatedQuery)
import qualified Database.Relational.Monad.Aggregate as Aggregate
import Database.Relational.Monad.Unique (QueryUnique, unsafeUniqueSubQuery, liftToQueryUnique)
import qualified Database.Relational.Monad.Unique as Unique
import Database.Relational.Table (Table, TableDerivable, derivedTable)
import qualified Database.Relational.Table as Table
import Database.Relational.Scalar (ScalarDegree)
import Database.Relational.Pi (Pi)
import Database.Relational.Record (RecordList)
import qualified Database.Relational.Record as Record
import Database.Relational.Projectable
  (PlaceHolders, unitPlaceHolder, unsafeAddPlaceHolders, unsafePlaceHolders, )

import Prelude hiding ((>>=), (>>))
import qualified Prelude


-- | Simple 'Relation' from 'Table'.
table :: Table r -> Relation (ExRecord '[]) j () r
table = unsafeTypeRelation . return . Table.toSubQuery

-- | Inferred 'Relation'. igrep TODO
derivedRelation :: TableDerivable r => Relation (ExRecord '[]) (ExRecord '[]) () r
derivedRelation =  table derivedTable

-- | Interface to derive 'Table' type object.
tableOf :: TableDerivable r => Relation (ExRecord '[]) (ExRecord '[]) () r -> Table r
tableOf =  const derivedTable

placeHoldersFromRelation :: Relation (ExRecord xs) j p r -> PlaceHolders p
placeHoldersFromRelation =  const unsafePlaceHolders

-- | Join sub-query. Query result is not 'Maybe'.
query :: (MonadQualify ConfigureQuery m, MonadQuery m)
      => Relation (ExRecord ys) (ExRecord zs) () r
      -> Placeholders m (ExRecord xs) (ExRecord (xs ++ zs)) (Record (ExRecord '[]) (ExRecord '[]) Flat r)
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
           => Relation (ExRecord ys) (ExRecord zs) () r
           -> Placeholders m (ExRecord xs) (ExRecord (xs ++ zs)) (Record (ExRecord '[]) (ExRecord '[]) Flat (Maybe r))
queryMaybe =  fmap snd . queryMaybe'

queryList0 :: MonadQualify ConfigureQuery m
           => Relation (ExRecord ys) (ExRecord zs) p r
           -> Placeholders m (ExRecord xs) (ExRecord (xs ++ zs)) (RecordList (Record (ExRecord '[]) (ExRecord '[]) c) r)
queryList0 rel =
  addingPlaceholders rel
    $ liftQualify
    $ Record.unsafeListFromSubQuery
    <$> untypeRelation rel

-- | List sub-query, for /IN/ and /EXIST/ with place-holder parameter 'p'.
queryList' :: MonadQualify ConfigureQuery m
           => Relation (ExRecord ys) (ExRecord zs) p r
           -> Placeholders m (ExRecord xs) (ExRecord (xs ++ zs)) (PlaceHolders p, RecordList (Record (ExRecord '[]) (ExRecord '[]) c) r)
queryList' rel = do
  ql <- queryList0 rel
  ireturn (placeHoldersFromRelation rel, ql)
 where (>>=) = (ReboundSyntax.>>=)

-- | List sub-query, for /IN/ and /EXIST/.
queryList :: MonadQualify ConfigureQuery m
          => Relation (ExRecord ys) (ExRecord zs) () r
          -> Placeholders m (ExRecord xs) (ExRecord (xs ++ zs)) (RecordList (Record (ExRecord '[]) (ExRecord '[]) c) r)
queryList =  queryList0

addUnitPH :: Functor f => f t -> f (PlaceHolders (), t)
addUnitPH =  ((,) unitPlaceHolder <$>)

-- | Finalize 'QuerySimple' monad and generate 'Relation' with place-holder parameter 'p'.
relation' :: SimpleQuery i j p r -> Relation i j p r
relation' =  unsafeTypeRelation . Simple.toSubQuery

-- | Finalize 'QuerySimple' monad and generate 'Relation'.
relation :: QuerySimple i j (Record (ExRecord '[]) (ExRecord '[]) Flat r) -> Relation i j () r
relation =  relation' . addUnitPH

-- | Finalize 'QueryAggregate' monad and geneate 'Relation' with place-holder parameter 'p'.
aggregateRelation' :: AggregatedQuery i j p r -> Relation i j p r
aggregateRelation' =  unsafeTypeRelation . Aggregate.toSubQuery

-- | Finalize 'QueryAggregate' monad and geneate 'Relation'.
aggregateRelation :: QueryAggregate i j (Record (ExRecord '[]) (ExRecord '[]) Aggregated r) -> Relation i j () r
aggregateRelation =  aggregateRelation' . addUnitPH


-- | Unique relation type to compose scalar queries.
newtype UniqueRelation i j p c r =  Unique (Relation i j p r)

-- | Unsafely specify unique relation.
unsafeUnique :: Relation i j p r -> UniqueRelation i j p c r
unsafeUnique =  Unique

-- | Discard unique attribute.
unUnique :: UniqueRelation i j p c r -> Relation i j p r
unUnique (Unique r) = r

-- | Basic monadic join operation using 'MonadQuery'.
uniqueQueryWithAttr :: forall xs ys p c r. NodeAttr
                    -> UniqueRelation (ExRecord '[]) (ExRecord ys) p c r
                    -> QueryUnique (ExRecord xs) (ExRecord (xs ++ ys)) (PlaceHolders p, Record (ExRecord '[]) (ExRecord ys) c r)
uniqueQueryWithAttr attr = unsafeAddPlaceHolders . run where
  run :: UniqueRelation (ExRecord '[]) (ExRecord ys) p c r -> QueryUnique (ExRecord xs) (ExRecord (xs ++ ys)) (Record (ExRecord '[]) (ExRecord ys) c r)
  run rel = do
    q <- liftQualify $ act rel :: QueryUnique (ExRecord xs) (ExRecord xs) (Qualified SubQuery)
    liftToQueryUnique (addPlaceholders (unUnique rel))
    Record.unsafeChangeIndices . Record.unsafeChangeContext <$> unsafeUniqueSubQuery attr q
    where
      (>>=) :: forall m i j k a b. IxMonad m => m i j a -> (a -> m j k b) -> m i k b
      (>>=) = (ReboundSyntax.>>=)

      (>>) :: IxMonad m => m i j a -> m j k b -> m i k b
      (>>) = (ReboundSyntax.>>)

  act rel = do
    sq <- untypeRelation $ unUnique rel
    qualifyQuery sq
    where
      (>>=) :: forall m a b. Monad m => m a -> (a -> m b) -> m b
      (>>=) = (Prelude.>>=)

-- | Join unique sub-query with place-holder parameter 'p'.
uniqueQuery' :: UniqueRelation (ExRecord '[]) (ExRecord ys) p c r
             -> QueryUnique (ExRecord xs) (ExRecord (xs ++ ys)) (PlaceHolders p, Record (ExRecord '[]) (ExRecord ys) c r)
uniqueQuery' = uniqueQueryWithAttr Just'

-- | Join unique sub-query with place-holder parameter 'p'. Query result is 'Maybe'.
uniqueQueryMaybe' :: UniqueRelation (ExRecord '[]) (ExRecord ys) p c r
                  -> QueryUnique (ExRecord xs) (ExRecord (xs ++ ys)) (PlaceHolders p, Record (ExRecord '[]) (ExRecord ys) c (Maybe r))
uniqueQueryMaybe' pr =  do
  (ph, pj) <- uniqueQueryWithAttr Maybe pr
  ireturn (ph, Record.just pj)
 where
  (>>=) :: forall m i j k a b. IxMonad m => m i j a -> (a -> m j k b) -> m i k b
  (>>=) = (ReboundSyntax.>>=)

-- | Finalize 'QueryUnique' monad and generate 'UniqueRelation'.
uniqueRelation' :: QueryUnique i j (PlaceHolders p, Record i j c r) -> UniqueRelation i j p c r
uniqueRelation' =  unsafeUnique . unsafeTypeRelation . Unique.toSubQuery

-- | Aggregated 'UniqueRelation'.
aggregatedUnique :: forall xs ys ph r a b
                  . Relation (ExRecord xs) (ExRecord ys) ph r
                 -> Pi r a
                 -> (Record (ExRecord '[]) (ExRecord '[]) Flat a -> Record (ExRecord '[]) (ExRecord '[]) Aggregated b)
                 -> UniqueRelation (ExRecord xs) (ExRecord (xs ++ ys)) ph Flat b
aggregatedUnique rel k ag = unsafeUnique $ aggregateRelation' act
 where
  act :: AggregatedQuery (ExRecord xs) (ExRecord (xs ++ ys)) ph b
  act = do
    (ph, a) <- query' rel
    ireturn (ph, ag $ Record.wpi (relationWidth rel) a k)
  (>>=) :: forall m i j k a' b'. IxMonad m => m i j a' -> (a' -> m j k b') -> m i k b'
  (>>=) = (ReboundSyntax.>>=)

-- | Scalar sub-query with place-holder parameter 'p'.
queryScalar' :: (MonadQualify ConfigureQuery m, ScalarDegree r)
             => UniqueRelation (ExRecord xs) (ExRecord ys) p c r
             -> Placeholders m (ExRecord xs) (ExRecord ys) (PlaceHolders p, Record (ExRecord '[]) (ExRecord ys) c (Maybe r))
queryScalar' ur =
  forceSettingPlaceholders (unUnique ur)
    . unsafeAddPlaceHolders
    . liftQualify
    $ Record.unsafeFromScalarSubQuery
    <$> untypeRelation (unUnique ur)

-- | Scalar sub-query.
queryScalar :: (MonadQualify ConfigureQuery m, ScalarDegree r)
            => UniqueRelation (ExRecord xs) (ExRecord ys) () c r
            -> Placeholders m (ExRecord xs) (ExRecord ys) (Record (ExRecord '[]) (ExRecord ys) c (Maybe r))
queryScalar =  fmap snd . queryScalar'

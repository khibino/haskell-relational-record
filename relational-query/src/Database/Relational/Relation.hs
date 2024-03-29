{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.Relational.Relation
-- Copyright   : 2013-2019 Kei Hibino
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
  relationFromTable, derivedRelation, tableFromRelation,
  relation, relation',
  aggregateRelation, aggregateRelation',

  UniqueRelation,
  unsafeUnique, unUnique,

  uniqueRelation', aggregatedUnique,

  -- * Query using relation
  query, queryMaybe, queryList, queryList', queryScalar, queryScalar',
  uniqueQuery', uniqueQueryMaybe',

  -- * Deprecated
  table, tableOf,
  ) where

import Control.Applicative ((<$>))

import Database.Relational.Internal.ContextType (Flat, Aggregated)
import Database.Relational.SqlSyntax (NodeAttr(Just', Maybe))
import Database.Relational.Typed.Table (Table, TableDerivable, derivedTable)
import qualified Database.Relational.Typed.Table as Table
import Database.Relational.Typed.Record
  (Record, unsafeRecordFromScalarQuery, RecordList)
import qualified Database.Relational.Typed.Record as Record

import Database.Relational.Monad.BaseType
  (ConfigureQuery, qualifyQuery,
   Relation, unsafeTypeRelation, untypeRelation, relationWidth)
import Database.Relational.Monad.Class
  (MonadQualify (liftQualify), MonadQuery (query', queryMaybe'), )
import Database.Relational.Monad.Simple (QuerySimple, SimpleQuery)
import qualified Database.Relational.Monad.Simple as Simple
import Database.Relational.Monad.Aggregate (QueryAggregate, AggregatedQuery)
import qualified Database.Relational.Monad.Aggregate as Aggregate
import Database.Relational.Monad.Unique (QueryUnique, unsafeUniqueSubQuery)
import qualified Database.Relational.Monad.Unique as Unique
import Database.Relational.Scalar (ScalarDegree)
import Database.Relational.Pi (Pi)
import qualified Database.Relational.Record as Record (wpi)
import Database.Relational.Projectable
  (PlaceHolders, unitPlaceHolder, unsafeAddPlaceHolders, unsafePlaceHolders, )


-- | Simple 'Relation' from 'Table'.
relationFromTable :: Table r -> Relation () r
relationFromTable = unsafeTypeRelation . return . Table.toSubQuery

-- | Simple 'Relation' from 'Table'.
table :: Table r -> Relation () r
table = relationFromTable
{-# DEPRECATED table "use relationFromTable instead of this." #-}

-- | Inferred 'Relation'.
derivedRelation :: TableDerivable r => Relation () r
derivedRelation =  relationFromTable derivedTable

-- | Interface to derive 'Table' type object.
tableFromRelation :: TableDerivable r => Relation () r -> Table r
tableFromRelation = const derivedTable

-- | Interface to derive 'Table' type object.
tableOf :: TableDerivable r => Relation () r -> Table r
tableOf = tableFromRelation
{-# DEPRECATED tableOf "use tableFromRelation instead of this." #-}

placeHoldersFromRelation :: Relation p r -> PlaceHolders p
placeHoldersFromRelation =  const unsafePlaceHolders

-- | Join sub-query. Query result is not 'Maybe'.
query :: (MonadQualify ConfigureQuery m, MonadQuery m)
      => Relation () r
      -> m (Record Flat r)
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
           -> m (Record Flat (Maybe r))
queryMaybe =  fmap snd . queryMaybe'

queryList0 :: MonadQualify ConfigureQuery m => Relation p r -> m (RecordList (Record c) r)
queryList0 =  liftQualify
              . fmap Record.unsafeListFromSubQuery
              . untypeRelation

-- | List sub-query, for /IN/ and /EXIST/ with place-holder parameter 'p'.
queryList' :: MonadQualify ConfigureQuery m
           => Relation p r
           -> m (PlaceHolders p, RecordList (Record c) r)
queryList' rel = do
  ql <- queryList0 rel
  return (placeHoldersFromRelation rel, ql)

-- | List sub-query, for /IN/ and /EXIST/.
queryList :: MonadQualify ConfigureQuery m
          => Relation () r
          -> m (RecordList (Record c) r)
queryList =  queryList0

addUnitPH :: Functor f => f t -> f (PlaceHolders (), t)
addUnitPH =  ((,) unitPlaceHolder <$>)

-- | Finalize 'QuerySimple' monad and generate 'Relation' with place-holder parameter 'p'.
relation' :: SimpleQuery p r -> Relation p r
relation' =  unsafeTypeRelation . Simple.toSubQuery

-- | Finalize 'QuerySimple' monad and generate 'Relation'.
relation :: QuerySimple (Record Flat r) -> Relation () r
relation =  relation' . addUnitPH

-- | Finalize 'QueryAggregate' monad and generate 'Relation' with place-holder parameter 'p'.
aggregateRelation' :: AggregatedQuery p r -> Relation p r
aggregateRelation' =  unsafeTypeRelation . Aggregate.toSubQuery

-- | Finalize 'QueryAggregate' monad and generate 'Relation'.
aggregateRelation :: QueryAggregate (Record Aggregated r) -> Relation () r
aggregateRelation =  aggregateRelation' . addUnitPH


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
                    -> QueryUnique (PlaceHolders p, Record c r)
uniqueQueryWithAttr attr = unsafeAddPlaceHolders . run where
  run rel = do
    q <- liftQualify $ do
      sq <- untypeRelation (unUnique rel)
      qualifyQuery sq
    Record.unsafeChangeContext <$> unsafeUniqueSubQuery attr q

-- | Join unique sub-query with place-holder parameter 'p'.
uniqueQuery' :: UniqueRelation p c r
             -> QueryUnique (PlaceHolders p, Record c r)
uniqueQuery' = uniqueQueryWithAttr Just'

-- | Join unique sub-query with place-holder parameter 'p'. Query result is 'Maybe'.
uniqueQueryMaybe' :: UniqueRelation p c r
                  -> QueryUnique (PlaceHolders p, Record c (Maybe r))
uniqueQueryMaybe' pr =  do
  (ph, pj) <- uniqueQueryWithAttr Maybe pr
  return (ph, Record.just pj)

-- | Finalize 'QueryUnique' monad and generate 'UniqueRelation'.
uniqueRelation' :: QueryUnique (PlaceHolders p, Record c r) -> UniqueRelation p c r
uniqueRelation' =  unsafeUnique . unsafeTypeRelation . Unique.toSubQuery

-- | Aggregated 'UniqueRelation'.
aggregatedUnique :: Relation ph r
                 -> Pi r a
                 -> (Record Flat a -> Record Aggregated b)
                 -> UniqueRelation ph Flat b
aggregatedUnique rel k ag = unsafeUnique . aggregateRelation' $ do
  (ph, a) <- query' rel
  return (ph, ag $ Record.wpi (relationWidth rel) a k)

-- | Scalar sub-query with place-holder parameter 'p'.
queryScalar' :: (MonadQualify ConfigureQuery m, ScalarDegree r)
             => UniqueRelation p c r
             -> m (PlaceHolders p, Record c (Maybe r))
queryScalar' ur =
  unsafeAddPlaceHolders . liftQualify $
  unsafeRecordFromScalarQuery <$> untypeRelation (unUnique ur)

-- | Scalar sub-query.
queryScalar :: (MonadQualify ConfigureQuery m, ScalarDegree r)
            => UniqueRelation () c r
            -> m (Record c (Maybe r))
queryScalar =  fmap snd . queryScalar'

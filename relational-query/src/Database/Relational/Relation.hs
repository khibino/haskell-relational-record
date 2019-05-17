{-# LANGUAGE FlexibleContexts #-}

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

import Database.Record (PersistableWidth)
import Database.Relational.Internal.ContextType (Flat, Aggregated, PureOperand)
import Database.Relational.SqlSyntax (NodeAttr(Just', Maybe), Record, )

import Database.Relational.Monad.BaseType
  (ConfigureQuery, qualifyQuery,
   Relation, unsafeTypeRelation, untypeRelation, relationWidth)
import Database.Relational.Monad.Class
  (MonadQualify (liftQualify), MonadQuery (query', queryMaybe'), )
import Database.Relational.Monad.Simple (QuerySimple)
import Database.Relational.Monad.Trans.ReadPlaceholders (readPlaceholders, askPlaceholders)
import qualified Database.Relational.Monad.Simple as Simple
import Database.Relational.Monad.Aggregate (QueryAggregate, AggregatedQuery)
import qualified Database.Relational.Monad.Aggregate as Aggregate
import Database.Relational.Monad.Unique (QueryUnique, unsafeUniqueSubQuery)
import qualified Database.Relational.Monad.Unique as Unique
import Database.Relational.Table (Table, TableDerivable, derivedTable)
import qualified Database.Relational.Table as Table
import Database.Relational.Scalar (ScalarDegree)
import Database.Relational.Pi (Pi)
import Database.Relational.Record (RecordList, pempty)
import qualified Database.Relational.Record as Record


-- | Simple 'Relation' from 'Table'.
table :: Table r -> Relation () r
table = unsafeTypeRelation . return . Table.toSubQuery

-- | Inferred 'Relation'.
derivedRelation :: TableDerivable r => Relation () r
derivedRelation =  table derivedTable

-- | Interface to derive 'Table' type object.
tableOf :: TableDerivable r => Relation () r -> Table r
tableOf =  const derivedTable

-- | Join sub-query. Query result is not 'Maybe'.
query :: (MonadQualify ConfigureQuery m, MonadQuery m)
      => Relation () r
      -> m (Record Flat r)
query =  query' pempty

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
queryMaybe =  queryMaybe' pempty

queryList0 :: MonadQualify ConfigureQuery m => Record PureOperand p -> Relation p r -> m (RecordList (Record c) r)
queryList0 phs = liftQualify
               . fmap Record.unsafeListFromSubQuery
               . (`untypeRelation` phs)

-- | List sub-query, for /IN/ and /EXIST/ with place-holder parameter 'p'.
queryList' :: MonadQualify ConfigureQuery m
           => Record PureOperand p
           -> Relation p r
           -> m (RecordList (Record c) r)
queryList' = queryList0

-- | List sub-query, for /IN/ and /EXIST/.
queryList :: MonadQualify ConfigureQuery m
          => Relation () r
          -> m (RecordList (Record c) r)
queryList =  queryList0 pempty

-- | Finalize 'QuerySimple' monad and generate 'Relation' with place-holder parameter 'p'.
relation' :: PersistableWidth p => (Record PureOperand p -> QuerySimple (Record Flat r)) -> Relation p r
relation' f  = unsafeTypeRelation $ readPlaceholders . Simple.toSubQuery . f =<< askPlaceholders

-- | Finalize 'QuerySimple' monad and generate 'Relation'.
relation :: QuerySimple (Record Flat r) -> Relation () r
relation =  relation' . const

-- | Finalize 'QueryAggregate' monad and geneate 'Relation' with place-holder parameter 'p'.
aggregateRelation' :: PersistableWidth p => (Record PureOperand p -> AggregatedQuery r) -> Relation p r
aggregateRelation' f  = unsafeTypeRelation $ readPlaceholders . Aggregate.toSubQuery . f =<< askPlaceholders

-- | Finalize 'QueryAggregate' monad and geneate 'Relation'.
aggregateRelation :: QueryAggregate (Record Aggregated r) -> Relation () r
aggregateRelation =  aggregateRelation' . const


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
                    -> Record PureOperand p
                    -> UniqueRelation p c r
                    -> QueryUnique (Record c r)
uniqueQueryWithAttr attr phs rel = do
  q <- liftQualify $ do
    sq <- untypeRelation (unUnique rel) phs
    qualifyQuery sq
  Record.unsafeChangeContext <$> unsafeUniqueSubQuery attr q

-- | Join unique sub-query with place-holder parameter 'p'.
uniqueQuery' :: Record PureOperand p
             -> UniqueRelation p c r
             -> QueryUnique (Record c r)
uniqueQuery' = uniqueQueryWithAttr Just'

-- | Join unique sub-query with place-holder parameter 'p'. Query result is 'Maybe'.
uniqueQueryMaybe' :: Record PureOperand p
                  -> UniqueRelation p c r
                  -> QueryUnique (Record c (Maybe r))
uniqueQueryMaybe' phs pr =  do
  pj <- uniqueQueryWithAttr Maybe phs pr
  return (Record.just pj)

-- | Finalize 'QueryUnique' monad and generate 'UniqueRelation'.
uniqueRelation' :: PersistableWidth p => (Record PureOperand p -> QueryUnique (Record c r)) -> UniqueRelation p c r
uniqueRelation' f  = unsafeUnique . unsafeTypeRelation $ readPlaceholders . Unique.toSubQuery . f =<< askPlaceholders

-- | Aggregated 'UniqueRelation'.
aggregatedUnique :: PersistableWidth ph
                 => Relation ph r
                 -> Pi r a
                 -> (Record Flat a -> Record Aggregated b)
                 -> UniqueRelation ph Flat b
aggregatedUnique rel k ag = unsafeUnique . aggregateRelation' $ \phs -> do
  a <- query' phs rel
  return (ag $ Record.wpi (relationWidth rel phs) a k)

-- | Scalar sub-query with place-holder parameter 'p'.
queryScalar' :: (MonadQualify ConfigureQuery m, ScalarDegree r)
             => Record PureOperand p
             -> UniqueRelation p c r
             -> m (Record c (Maybe r))
queryScalar' phs ur =
  liftQualify $ Record.unsafeFromScalarSubQuery <$> untypeRelation (unUnique ur) phs

-- | Scalar sub-query.
queryScalar :: (MonadQualify ConfigureQuery m, ScalarDegree r)
            => UniqueRelation () c r
            -> m (Record c (Maybe r))
queryScalar =  queryScalar' pempty

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Monad.Aggregate
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains definitions about aggregated query type.
module Database.Relational.Monad.Aggregate (
  -- * Aggregated Query
  QueryAggregate,
  AggregatedQuery,

  toSQL,

  toSubQuery,

  Window, over
  ) where

import Data.Functor.Identity (Identity (runIdentity))
import Data.Monoid ((<>))

import Language.SQL.Keyword (Keyword(..))
import qualified Language.SQL.Keyword as SQL

import Database.Relational.Internal.ContextType (Flat, Aggregated, OverWindow)
import Database.Relational.SqlSyntax
  (Duplication, Record, SubQuery, Tuple, JoinProduct,
   OrderingTerm, composeOrderBy, aggregatedSubQuery,
   AggregateColumnRef, AggregateElem, composePartitionBy, )
import qualified Database.Relational.SqlSyntax as Syntax

import qualified Database.Relational.Record as Record
import Database.Relational.Projectable (PlaceHolders, SqlContext)
import Database.Relational.Monad.Class (MonadRestrict(..))
import Database.Relational.Monad.Trans.Restricting
  (Restrictings, restrictings, extractRestrict)
import Database.Relational.Monad.Trans.Aggregating
  (extractAggregateTerms, AggregatingSetT, PartitioningSet)
import Database.Relational.Monad.Trans.Ordering
  (Orderings, extractOrderingTerms)
import Database.Relational.Monad.BaseType (ConfigureQuery, askConfig)
import Database.Relational.Monad.Type (QueryCore, extractCore, OrderedQuery)


-- | Aggregated query monad type.
type QueryAggregate     = Orderings Aggregated (Restrictings Aggregated (AggregatingSetT QueryCore))

-- | Aggregated query type. 'AggregatedQuery' p r == 'QueryAggregate' ('PlaceHolders' p, 'Record' 'Aggregated' r).
type AggregatedQuery i j p r = OrderedQuery i j Aggregated (Restrictings Aggregated (AggregatingSetT QueryCore)) p r

-- | Partition monad type for partition-by clause.
type Window           c = Orderings c (PartitioningSet c)

-- | Restricted 'MonadRestrict' instance.
instance MonadRestrict Flat q => MonadRestrict Flat (Restrictings Aggregated q) where
  restrict = restrictings . restrict

extract :: AggregatedQuery i j p r
        -> ConfigureQuery (((((((PlaceHolders p, Record i j Aggregated r), [OrderingTerm]),
                               [Tuple{-Predicate i j Aggregated-}]),
                              [AggregateElem]),
                             [Tuple{-Predicate i j Flat-}]),
                            JoinProduct), Duplication)
extract =  extractCore . extractAggregateTerms . extractRestrict . extractOrderingTerms

-- | Run 'AggregatedQuery' to get SQL with 'ConfigureQuery' computation.
toSQL :: AggregatedQuery i j p r   -- ^ 'AggregatedQuery' to run
      -> ConfigureQuery String -- ^ Result SQL string with 'ConfigureQuery' computation
toSQL =  fmap Syntax.toSQL . toSubQuery

-- | Run 'AggregatedQuery' to get 'SubQuery' with 'ConfigureQuery' computation.
toSubQuery :: AggregatedQuery i j p r       -- ^ 'AggregatedQuery' to run
           -> ConfigureQuery SubQuery -- ^ Result 'SubQuery' with 'ConfigureQuery' computation
toSubQuery q = do
  (((((((_ph, pj), ot), grs), ag), rs), pd), da) <- extract q
  c <- askConfig
  return $ aggregatedSubQuery c (Record.untype pj) da pd rs ag grs ot

extractWindow :: Window c a -> ((a, [OrderingTerm]), [AggregateColumnRef])
extractWindow =  runIdentity . extractAggregateTerms . extractOrderingTerms

-- | Operator to make record of window function result using built 'Window' monad.
over :: SqlContext c
     => Record i j OverWindow a
     -> Window c ()
     -> Record i j c a
wp `over` win =
  Record.unsafeFromSqlTerms
  [ c <> OVER <> SQL.paren (composePartitionBy pt <> composeOrderBy ot)
  | c <- Record.columns wp
  ]  where (((), ot), pt) = extractWindow win

infix 8 `over`

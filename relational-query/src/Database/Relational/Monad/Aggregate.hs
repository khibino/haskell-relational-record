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

import Data.DList (DList)
import Data.Functor.Identity (Identity (runIdentity))
import Data.Monoid ((<>))

import Language.SQL.Keyword (Keyword(..))
import qualified Language.SQL.Keyword as SQL

import Database.Relational.Internal.ContextType (Flat, Aggregated, OverWindow)
import Database.Relational.SqlSyntax
  (Duplication, Record, SubQuery, Predicate, JoinProduct,
   OrderingTerm, composeOrderBy, aggregatedSubQuery,
   AggregateColumnRef, AggregateElem, composePartitionBy, placeholderOffsets, )
import qualified Database.Relational.SqlSyntax as Syntax

import qualified Database.Relational.Record as Record
import Database.Relational.Projectable (PlaceHolders, SqlContext)
import Database.Relational.Monad.Class (MonadRestrict(..))
import Database.Relational.Monad.Trans.Restricting
  (Restrictings, restrictings, extractRestrict)
import Database.Relational.Monad.Trans.ReferredPlaceholders (ReferredPlaceholders, extractReferredPlaceholders)
import Database.Relational.Monad.Trans.Aggregating
  (extractAggregateTerms, AggregatingSetT, PartitioningSet)
import Database.Relational.Monad.Trans.Ordering
  (Orderings, extractOrderingTerms)
import Database.Relational.Monad.BaseType (ConfigureQuery, askConfig)
import Database.Relational.Monad.Type (QueryCore, extractCore)


-- | Aggregated query monad type.
type QueryAggregate      = ReferredPlaceholders (Orderings Aggregated (Restrictings Aggregated (AggregatingSetT QueryCore)))

-- | Aggregated query type with placeholders.
type AggregatedQuery p r = ReferredPlaceholders (Orderings Aggregated (Restrictings Aggregated (AggregatingSetT QueryCore))) (PlaceHolders p, Record Aggregated r)

-- | Partition monad type for partition-by clause.
type Window           c = ReferredPlaceholders (Orderings c (PartitioningSet c))

-- | Restricted 'MonadRestrict' instance.
instance MonadRestrict Flat q => MonadRestrict Flat (Restrictings Aggregated q) where
  restrict = restrictings . restrict

extract :: AggregatedQuery p r
        -> ConfigureQuery ((((((((PlaceHolders p, Record Aggregated r),
                                 DList Int),
                                [OrderingTerm]),
                               [Predicate Aggregated]),
                              [AggregateElem]),
                             [Predicate Flat]),
                            JoinProduct), Duplication)
extract =  extractCore . extractAggregateTerms . extractRestrict . extractOrderingTerms . extractReferredPlaceholders

-- | Run 'AggregatedQuery' to get SQL with 'ConfigureQuery' computation.
toSQL :: AggregatedQuery p r   -- ^ 'AggregatedQuery' to run
      -> ConfigureQuery String -- ^ Result SQL string with 'ConfigureQuery' computation
toSQL =  fmap (Syntax.toSQL . snd) . toSubQuery

-- | Run 'AggregatedQuery' to get 'SubQuery' with 'ConfigureQuery' computation.
toSubQuery :: AggregatedQuery p r                  -- ^ 'AggregatedQuery' to run
           -> ConfigureQuery (DList Int, SubQuery) -- ^ Result 'SubQuery' with 'ConfigureQuery' computation
toSubQuery q = do
  ((((((((_ph, pj), phs), ot), grs), ag), rs), pd), da) <- extract q
  c <- askConfig
  return (phs, aggregatedSubQuery c (Record.untype pj) da pd rs ag grs ot)

extractWindow :: Window c a -> (((a, DList Int), [OrderingTerm]), [AggregateColumnRef])
extractWindow =  runIdentity . extractAggregateTerms . extractOrderingTerms . extractReferredPlaceholders

-- | Operator to make record of window function result using built 'Window' monad.
over :: SqlContext c
     => Record OverWindow a
     -> Window c ()
     -> Record c a
wp `over` win =
  Record.unsafeFromSqlTerms
  (placeholderOffsets wp <> phs)
  [ c <> OVER <> SQL.paren (composePartitionBy pt <> composeOrderBy ot)
  | c <- Record.columns wp
  ]  where ((((), phs), ot), pt) = extractWindow win

infix 8 `over`

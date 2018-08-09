{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
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

import Database.Relational.ExtensibleRecord
import qualified Database.Relational.Record as Record
import Database.Relational.Projectable (PlaceHolders, SqlContext)
import Database.Relational.Monad.Class (MonadRestrict(..))
import Database.Relational.Monad.Trans.Restricting
  (Restrictings, restrictings, extractRestrict)
import Database.Relational.Monad.Trans.Aggregating
  (extractAggregateTerms, AggregatingSetT, PartitioningSet)
import Database.Relational.Monad.Trans.Ordering
  (Orderings, extractOrderingTerms)
import qualified Database.Relational.Monad.Trans.Placeholders as P 
import Database.Relational.Monad.BaseType (ConfigureQuery, askConfig)
import Database.Relational.Monad.Type (QueryCore, extractCore, OrderedQuery)


-- | Aggregated query monad type.
type QueryAggregate     = P.Placeholders (Orderings Aggregated (Restrictings Aggregated (AggregatingSetT QueryCore)))

-- | Aggregated query type. 'AggregatedQuery' p r == 'QueryAggregate' ('PlaceHolders' p, 'Record' 'Aggregated' r).
type AggregatedQuery i j p r = OrderedQuery i j Aggregated (Restrictings Aggregated (AggregatingSetT QueryCore)) p r

-- | Partition monad type for partition-by clause.
type Window            c i j = P.Placeholders (Orderings c (PartitioningSet c)) i j

-- | Restricted 'MonadRestrict' instance.
instance MonadRestrict Flat q => MonadRestrict Flat (Restrictings Aggregated q) where
  restrict = restrictings . restrict

extract :: AggregatedQuery i j p r
        -> ConfigureQuery (((((((PlaceHolders p, Record (ExRecord '[]) (ExRecord '[]) Aggregated r), [OrderingTerm]),
                               [Tuple{-Predicate i j Aggregated-}]),
                              [AggregateElem]),
                             [Tuple{-Predicate i j Flat-}]),
                            JoinProduct), Duplication)
extract =  extractCore . extractAggregateTerms . extractRestrict . extractOrderingTerms . P.extractPlaceholders

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

extractWindow :: Window c i j a -> ((a, [OrderingTerm]), [AggregateColumnRef])
extractWindow =  runIdentity . extractAggregateTerms . extractOrderingTerms . P.extractPlaceholders

-- | Operator to make record of window function result using built 'Window' monad.
over :: SqlContext c
     => Record i j OverWindow a
     -> Window c i j ()
     -> Record i j c a
wp `over` win =
  Record.unsafeFromSqlTerms
  [ c <> OVER <> SQL.paren (composePartitionBy pt <> composeOrderBy ot)
  | c <- Record.columns wp
  ]  where (((), ot), pt) = extractWindow win

infix 8 `over`

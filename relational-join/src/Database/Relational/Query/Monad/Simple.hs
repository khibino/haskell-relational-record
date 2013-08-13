{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Query.Monad.Simple
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines definitions about simple (not-aggregated) query type.
module Database.Relational.Query.Monad.Simple (
  -- * Simple query
  QuerySimple, SimpleQuery,

  simple,

  toSQL,
  toSubQuery
  ) where

import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.SQL (selectSeedSQL)

import Database.Relational.Query.Monad.Qualify (Qualify)
import Database.Relational.Query.Monad.Class (MonadQualify(..))
import Database.Relational.Query.Monad.Trans.Join
  (join', FromAppend, runFromAppend, appendFrom)
import Database.Relational.Query.Monad.Trans.Ordering
  (Orderings, orderings, OrderedQuery, OrderByAppend, orderByAppend, appendOrderBys)
import Database.Relational.Query.Monad.Trans.Restrict
  (restrict, WheresAppend, wheresAppend, appendWheres)
import Database.Relational.Query.Monad.Core (QueryCore)

import Database.Relational.Query.Sub (SubQuery, subQuery)


-- | Simple query (not-aggregated) monad type.
type QuerySimple = Orderings Projection QueryCore

-- | Simple query (not-aggregated) query type. SimpleQuery r == QuerySimple (Projection r).
type SimpleQuery r = OrderedQuery Projection QueryCore r

-- | Lift from qualified table forms into 'QuerySimple'.
simple :: Qualify a -> QuerySimple a
simple =  orderings . restrict . join'

-- | Instance to lift from qualified table forms into 'QuerySimple'.
instance MonadQualify Qualify (Orderings Projection QueryCore) where
  liftQualify = simple

expandAppend :: SimpleQuery r
              -> Qualify (((Projection r, OrderByAppend), WheresAppend), FromAppend)
expandAppend =  appendFrom . appendWheres . appendOrderBys

-- | Run 'SimpleQuery' to get SQL string.
expandSQL :: SimpleQuery r -> Qualify (String, Projection r)
expandSQL q = do
  (((pj, ao), aw), af) <- expandAppend q
  return (orderByAppend ao . wheresAppend aw . runFromAppend af $ selectSeedSQL pj, pj)

-- | Run 'SimpleQuery' to get SQL string with 'Qualify' computation.
toSQL :: SimpleQuery r  -- ^ 'SimpleQuery' to run
      -> Qualify String -- ^ Result SQL string with 'Qualify' computation
toSQL q = fst `fmap` expandSQL q

-- | Run 'SimpleQuery' to get 'SubQuery' with 'Qualify' computation.
toSubQuery :: SimpleQuery r    -- ^ 'SimpleQuery' to run
           -> Qualify SubQuery -- ^ Result 'SubQuery' with 'Qualify' computation
toSubQuery q = do
  (sql, pj) <- expandSQL q
  return $ subQuery sql (Projection.width pj)

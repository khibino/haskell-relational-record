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
-- This module contains definitions about simple (not-aggregated) query type.
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
  (join', FromPrepend, prependFrom, extractFrom)
import Database.Relational.Query.Monad.Trans.Restricting
  (restrict, WherePrepend, prependWhere, extractWheres)
import Database.Relational.Query.Monad.Trans.Ordering
  (Orderings, orderings, OrderedQuery, OrderByPrepend, prependOrderBy, extractOrderBys)
import Database.Relational.Query.Monad.Type (QueryCore)

import Database.Relational.Query.Sub (SubQuery, subQuery)


-- | Simple query (not-aggregated) monad type.
type QuerySimple = Orderings Projection QueryCore

-- | Simple query (not-aggregated) query type. 'SimpleQuery' r == 'QuerySimple' ('Projection' r).
type SimpleQuery r = OrderedQuery Projection QueryCore r

-- | Lift from qualified table forms into 'QuerySimple'.
simple :: Qualify a -> QuerySimple a
simple =  orderings . restrict . join'

-- | Instance to lift from qualified table forms into 'QuerySimple'.
instance MonadQualify Qualify (Orderings Projection QueryCore) where
  liftQualify = simple

expandPrepend :: SimpleQuery r
              -> Qualify (((Projection r, OrderByPrepend), WherePrepend), FromPrepend)
expandPrepend =  extractFrom . extractWheres . extractOrderBys

-- | Run 'SimpleQuery' to get SQL string.
expandSQL :: SimpleQuery r -> Qualify (String, Projection r)
expandSQL q = do
  (((pj, ao), aw), af) <- expandPrepend q
  return (selectSeedSQL pj . prependFrom af . prependWhere aw . prependOrderBy ao $ "", pj)

-- | Run 'SimpleQuery' to get SQL string with 'Qualify' computation.
toSQL :: SimpleQuery r  -- ^ 'SimpleQuery' to run
      -> Qualify String -- ^ Result SQL string with 'Qualify' computation
toSQL =  fmap fst . expandSQL

-- | Run 'SimpleQuery' to get 'SubQuery' with 'Qualify' computation.
toSubQuery :: SimpleQuery r    -- ^ 'SimpleQuery' to run
           -> Qualify SubQuery -- ^ Result 'SubQuery' with 'Qualify' computation
toSubQuery q = do
  (sql, pj) <- expandSQL q
  return $ subQuery sql (Projection.width pj)

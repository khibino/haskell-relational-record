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

import Database.Relational.Query.Context (Flat)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection

import Database.Relational.Query.Monad.Class (MonadQualify(..))
import Database.Relational.Query.Monad.Trans.Config (askConfig)
import Database.Relational.Query.Monad.Trans.Join (join')
import Database.Relational.Query.Monad.Trans.Restricting (restrictings)
import Database.Relational.Query.Monad.Trans.Ordering
  (Orderings, orderings, OrderedQuery, extractOrderingTerms)
import Database.Relational.Query.Monad.Type (ConfigureQuery, QueryCore, extractCore)

import Database.Relational.Query.Component (QueryRestriction, OrderingTerms)
import Database.Relational.Query.Sub (SubQuery, flatSubQuery, JoinProduct)
import qualified Database.Relational.Query.Sub as SubQuery


-- | Simple query (not-aggregated) monad type.
type QuerySimple = Orderings Flat QueryCore

-- | Simple query (not-aggregated) query type. 'SimpleQuery' r == 'QuerySimple' ('Projection' r).
type SimpleQuery r = OrderedQuery Flat QueryCore r

-- | Lift from qualified table forms into 'QuerySimple'.
simple :: ConfigureQuery a -> QuerySimple a
simple =  orderings . restrictings . join'

-- | Instance to lift from qualified table forms into 'QuerySimple'.
instance MonadQualify ConfigureQuery (Orderings Flat QueryCore) where
  liftQualify = simple

extract :: SimpleQuery r
        -> ConfigureQuery (((Projection Flat r, OrderingTerms), QueryRestriction Flat), JoinProduct)
extract =  extractCore . extractOrderingTerms

-- | Run 'SimpleQuery' to get SQL string with 'Qualify' computation.
toSQL :: SimpleQuery r         -- ^ 'SimpleQuery' to run
      -> ConfigureQuery String -- ^ Result SQL string with 'Qualify' computation
toSQL =  fmap SubQuery.toSQL . toSubQuery

-- | Run 'SimpleQuery' to get 'SubQuery' with 'Qualify' computation.
toSubQuery :: SimpleQuery r           -- ^ 'SimpleQuery' to run
           -> ConfigureQuery SubQuery -- ^ Result 'SubQuery' with 'Qualify' computation
toSubQuery q = do
   (((pj, ot), rs), pd) <- extract q
   c <- askConfig
   return $ flatSubQuery c (Projection.untype pj) pd rs ot

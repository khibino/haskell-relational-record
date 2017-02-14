{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Query.Monad.Simple
-- Copyright   : 2013-2017 Kei Hibino
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
  toSubQuery,
  ) where

import Database.Relational.Query.Context (Flat)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection

import Database.Relational.Query.Internal.BaseSQL (Duplication, OrderingTerm)

import Database.Relational.Query.Monad.Trans.Join (join')
import Database.Relational.Query.Monad.Trans.Restricting (restrictings)
import Database.Relational.Query.Monad.Trans.Ordering
  (Orderings, orderings, extractOrderingTerms)
import Database.Relational.Query.Monad.BaseType (ConfigureQuery, askConfig)
import Database.Relational.Query.Monad.Type (QueryCore, extractCore, OrderedQuery)
import Database.Relational.Query.Projectable (PlaceHolders)

import Database.Relational.Query.Sub (SubQuery, QueryRestriction, JoinProduct, flatSubQuery)
import qualified Database.Relational.Query.Sub as SubQuery


-- | Simple (not-aggregated) query monad type.
type QuerySimple = Orderings Flat QueryCore

-- | Simple (not-aggregated) query type. 'SimpleQuery'' p r == 'QuerySimple' ('PlaceHolders' p, 'Projection' r).
type SimpleQuery p r = OrderedQuery Flat QueryCore p r

-- | Lift from qualified table forms into 'QuerySimple'.
simple :: ConfigureQuery a -> QuerySimple a
simple =  orderings . restrictings . join'

extract :: SimpleQuery p r
        -> ConfigureQuery (((((PlaceHolders p, Projection Flat r), [OrderingTerm]), QueryRestriction Flat),
                           JoinProduct), Duplication)
extract =  extractCore . extractOrderingTerms

-- | Run 'SimpleQuery' to get SQL string with 'Qualify' computation.
toSQL :: SimpleQuery p r         -- ^ 'SimpleQuery' to run
      -> ConfigureQuery String -- ^ Result SQL string with 'Qualify' computation
toSQL =  fmap SubQuery.toSQL . toSubQuery

-- | Run 'SimpleQuery' to get 'SubQuery' with 'Qualify' computation.
toSubQuery :: SimpleQuery p r        -- ^ 'SimpleQuery'' to run
           -> ConfigureQuery SubQuery -- ^ Result 'SubQuery' with 'Qualify' computation
toSubQuery q = do
   (((((_ph, pj), ot), rs), pd), da) <- extract q
   c <- askConfig
   return $ flatSubQuery c (Projection.untype pj) da pd rs ot

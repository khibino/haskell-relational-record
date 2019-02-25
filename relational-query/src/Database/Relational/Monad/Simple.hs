{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Monad.Simple
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains definitions about simple (not-aggregated) query type.
module Database.Relational.Monad.Simple (
  -- * Simple query
  QuerySimple, SimpleQuery,

  simple,

  toSQL,
  toSubQuery,
  ) where

import Database.Relational.Internal.ContextType (Flat)
import Database.Relational.SqlSyntax
  (Duplication, OrderingTerm, JoinProduct, Predicate,  Record,
   PlaceholderOffsets, SubQuery, flatSubQuery, )
import qualified Database.Relational.SqlSyntax as Syntax

import qualified Database.Relational.Record as Record
import Database.Relational.Monad.Trans.Join (join')
import Database.Relational.Monad.Trans.ReferredPlaceholders
  (ReferredPlaceholders, extractReferredPlaceholders, referredPlaceholders)
import Database.Relational.Monad.Trans.Restricting (restrictings)
import Database.Relational.Monad.Trans.Ordering
  (Orderings, orderings, extractOrderingTerms)
import Database.Relational.Monad.BaseType (ConfigureQuery, askConfig)
import Database.Relational.Monad.Type (QueryCore, extractCore)
import Database.Relational.Projectable (PlaceHolders)


-- | Simple (not-aggregated) query monad type.
type QuerySimple     = ReferredPlaceholders (Orderings Flat QueryCore)

-- | Simple (not-aggregated) query type with placeholders.
type SimpleQuery p r = ReferredPlaceholders (Orderings Flat QueryCore) (PlaceHolders p, Record Flat r)

-- | Lift from qualified table forms into 'QuerySimple'.
simple :: ConfigureQuery a -> QuerySimple a
simple =  referredPlaceholders . orderings . restrictings . join'

extract :: SimpleQuery p r
        -> ConfigureQuery ((((((PlaceHolders p, Record Flat r), PlaceholderOffsets), [OrderingTerm]), [Predicate Flat]),
                           JoinProduct), Duplication)
extract =  extractCore . extractOrderingTerms . extractReferredPlaceholders

-- | Run 'SimpleQuery' to get SQL string with 'Qualify' computation.
toSQL :: SimpleQuery p r       -- ^ 'SimpleQuery' to run
      -> ConfigureQuery String -- ^ Result SQL string with 'Qualify' computation
toSQL =  fmap (Syntax.toSQL . snd) . toSubQuery

-- | Run 'SimpleQuery' to get 'SubQuery' with 'Qualify' computation.
toSubQuery :: SimpleQuery p r                      -- ^ 'SimpleQuery'' to run
           -> ConfigureQuery (PlaceholderOffsets, SubQuery) -- ^ Result 'SubQuery' with 'Qualify' computation
toSubQuery q = do
   ((((((_ph, pj), phs), ot), rs), pd), da) <- extract q
   c <- askConfig
   return (phs, flatSubQuery c (Record.untype pj) da pd rs ot)

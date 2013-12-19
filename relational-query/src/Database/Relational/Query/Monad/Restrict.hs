-- |
-- Module      : Database.Relational.Query.Monad.Restrict
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains definitions about simple restrict context monad type.
module Database.Relational.Query.Monad.Restrict (
  -- * Monad to restrict target records.
  Restrict, RestrictedStatement,

  -- restricted,
  extract
  ) where

import Data.Functor.Identity (Identity (..), runIdentity)

import Database.Relational.Query.Component (QueryRestriction)
import Database.Relational.Query.Context (Flat)
import Database.Relational.Query.Projection (Projection)
import Database.Relational.Query.Monad.Trans.Restricting (Restrictings, extractRestrict)


-- | Restrict only monad type used from update statement and delete statement.
type Restrict = Restrictings Flat Identity

-- | RestrictedStatement type synonym.
--   Projection record type 'r' must be
--   the same as 'Restrictings' type parameter 'r'.
type RestrictedStatement r a = Projection Flat r -> Restrict a

-- -- | 'return' of 'Restrict'
-- restricted :: a -> Restrict a
-- restricted =  restrict . Identity

-- | Run 'Restrict' to get 'QueryRestriction'.
extract :: Restrict a -> (a, QueryRestriction Flat)
extract =  runIdentity . extractRestrict

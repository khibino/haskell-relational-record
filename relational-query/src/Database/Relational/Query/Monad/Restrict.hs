{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Query.Monad.Restrict
-- Copyright   : 2013-2016 Kei Hibino
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

import Database.Relational.Query.Internal.Config (Config)
import Database.Relational.Query.Sub (QueryRestriction)
import Database.Relational.Query.Context (Flat)
import Database.Relational.Query.Projection (Projection)
import Database.Relational.Query.Monad.Trans.Restricting
  (Restrictings, extractRestrict)
import Database.Relational.Query.Monad.BaseType (ConfigureQuery, configureQuery)


-- | Restrict only monad type used from update statement and delete statement.
type Restrict = Restrictings Flat ConfigureQuery

-- | RestrictedStatement type synonym.
--   Projection record type 'r' must be
--   the same as 'Restrictings' type parameter 'r'.
type RestrictedStatement r a = Projection Flat r -> Restrict a

-- -- | 'return' of 'Restrict'
-- restricted :: a -> Restrict a
-- restricted =  restrict . Identity

-- | Run 'Restrict' to get 'QueryRestriction'.
extract :: Restrict a -> Config -> (a, QueryRestriction Flat)
extract =  configureQuery . extractRestrict

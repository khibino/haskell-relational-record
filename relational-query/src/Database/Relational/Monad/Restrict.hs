{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Monad.Restrict
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains definitions about simple restrict context monad type.
module Database.Relational.Monad.Restrict (
  -- * Monad to restrict target records.
  Restrict, RestrictNoPh, RestrictedStatement,

  -- restricted,
  extract,
  extractNoPh
  ) where

import Database.Relational.Internal.ContextType (Flat)
import Database.Relational.Internal.Config (Config)
import Database.Relational.SqlSyntax (Predicate, Record, PlaceholderOffsets)

import Database.Relational.Monad.BaseType (ConfigureQuery, configureQuery)
import Database.Relational.Monad.Trans.Restricting
  (Restrictings, extractRestrict)
import Database.Relational.Monad.Trans.ReferredPlaceholders (ReferredPlaceholders, extractReferredPlaceholders)


-- | Restrict only monad type used from update statement and delete statement.
type Restrict = ReferredPlaceholders RestrictNoPh

type RestrictNoPh = Restrictings Flat ConfigureQuery

-- | RestrictedStatement type synonym.
--   Record type 'r' must be
--   the same as 'Restrictings' type parameter 'r'.
type RestrictedStatement r a = Record Flat r -> Restrict a

-- | Run 'Restrict' to get 'QueryRestriction'.
extract :: Restrict a -> Config -> ((a, PlaceholderOffsets), [Predicate Flat])
extract =  configureQuery . extractRestrict . extractReferredPlaceholders

-- | Run 'Restrict' to get 'QueryRestriction'.
extractNoPh :: RestrictNoPh a -> Config -> (a, [Predicate Flat])
extractNoPh =  configureQuery . extractRestrict

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
  Restrict, RestrictedStatement,

  -- restricted,
  extract
  ) where

import Database.Relational.Internal.ContextType (Flat)
import Database.Relational.Internal.Config (Config)
import Database.Relational.SqlSyntax (Record, Tuple)

import Database.Relational.Monad.Trans.Restricting
  (Restrictings, extractRestrict)
import Database.Relational.Monad.BaseType (ConfigureQuery, configureQuery)


-- | Restrict only monad type used from update statement and delete statement.
type Restrict = Restrictings Flat ConfigureQuery

-- | RestrictedStatement type synonym.
--   Record type 'r' must be
--   the same as 'Restrictings' type parameter 'r'.
type RestrictedStatement i j r a = Record i j Flat r -> Restrict a

-- -- | 'return' of 'Restrict'
-- restricted :: a -> Restrict a
-- restricted =  restrict . Identity

-- | Run 'Restrict' to get 'QueryRestriction'.
extract :: Restrict a -> Config -> (a, [Tuple{-Predicate i j Flat-}])
extract =  configureQuery . extractRestrict

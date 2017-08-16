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

import Database.Relational.SqlSyntax (Config, QueryRestriction, Record)

import Database.Relational.Context (Flat)
import Database.Relational.Monad.Trans.Restricting
  (Restrictings, extractRestrict)
import Database.Relational.Monad.BaseType (ConfigureQuery, configureQuery)


-- | Restrict only monad type used from update statement and delete statement.
type Restrict = Restrictings Flat ConfigureQuery

-- | RestrictedStatement type synonym.
--   Record type 'r' must be
--   the same as 'Restrictings' type parameter 'r'.
type RestrictedStatement r a = Record Flat r -> Restrict a

-- -- | 'return' of 'Restrict'
-- restricted :: a -> Restrict a
-- restricted =  restrict . Identity

-- | Run 'Restrict' to get 'QueryRestriction'.
extract :: Restrict a -> Config -> (a, QueryRestriction Flat)
extract =  configureQuery . extractRestrict

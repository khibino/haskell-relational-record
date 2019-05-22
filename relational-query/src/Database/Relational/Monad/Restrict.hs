{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Monad.Restrict
-- Copyright   : 2013-2019 Kei Hibino
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
  extract
  ) where

import Database.Relational.Internal.ContextType (Flat)
import Database.Relational.Internal.Config (Config)
import Database.Relational.SqlSyntax (Tuple, Record, WithPlaceholderOffsets)

import Database.Relational.Monad.Trans.Restricting
  (Restrictings, extractRestrict)
import Database.Relational.Monad.Trans.ReadPlaceholders (ReadPlaceholders,)
import Database.Relational.Monad.BaseType (ConfigureQuery, configureQuery)


-- | Restrict only monad type used from update statement and delete statement.
type Restrict = Restrictings Flat ConfigureQuery

-- | RestrictedStatement type synonym.
--   Record type 'r' must be
--   the same as 'Restrictings' type parameter 'r'.
type RestrictedStatement p r a = Record Flat r -> ReadPlaceholders p Restrict a

-- | Run 'Restrict' to get 'QueryRestriction'.
extract :: Restrict a -> Config -> (a, [WithPlaceholderOffsets Tuple])
extract =  configureQuery . extractRestrict

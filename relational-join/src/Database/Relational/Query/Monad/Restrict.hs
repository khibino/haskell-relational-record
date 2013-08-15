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
  Restrict,

  -- restricted,
  expandPrepend
  ) where

import Data.Functor.Identity (Identity (..), runIdentity)

import Database.Relational.Query.Monad.Trans.Restrict
  (WherePrepend, extractWheres)
import qualified Database.Relational.Query.Monad.Trans.Restrict as Trans


-- | Restrict only monad type used from update statement and delete statement.
type Restrict = Trans.Restrict Identity

-- -- | 'return' of 'Restrict'
-- restricted :: a -> Restrict a
-- restricted =  restrict . Identity

expandPrepend :: Restrict a -> (a, WherePrepend)
expandPrepend =  runIdentity . extractWheres

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Database.Relational.Monad.Trans.Qualify
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines monad transformer which qualify uniquely SQL table forms.
--
-- This is not public interface.
module Database.Relational.Monad.Trans.Qualify (
  -- * Qualify monad
  Qualify, qualify,
  evalQualifyPrime, qualifyQuery
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, runStateT, get, modify)
import Control.Applicative (Applicative)
import Control.Monad (liftM, ap)

import qualified Database.Relational.Internal.Sub as Internal


-- | Monad type to qualify SQL table forms.
newtype Qualify m a =
  Qualify (StateT Int m a)
  deriving (Monad, Functor, Applicative)

-- | Run qualify monad with initial state to get only result.
evalQualifyPrime :: Monad m => Qualify m a -> m a
evalQualifyPrime (Qualify s) = fst `liftM` runStateT s 0 {- primary alias id -}

-- | Generated new qualifier on internal state.
newAlias :: Monad m => Qualify m Internal.Qualifier
newAlias =  Qualify $ do
  ai <- Internal.Qualifier `liftM` get
  modify (+ 1)
  return ai

-- | Lift to 'Qualify'
qualify :: Monad m => m a -> Qualify m a
qualify =  Qualify . lift

-- | Get qualifyed table form query.
qualifyQuery :: Monad m
             => query                                -- ^ Query to qualify
             -> Qualify m (Internal.Qualified query) -- ^ Result with updated state
qualifyQuery query =
  Internal.qualify `liftM` newAlias `ap` return query

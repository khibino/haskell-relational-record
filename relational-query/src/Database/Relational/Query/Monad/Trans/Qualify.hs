{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Database.Relational.Query.Monad.Trans.Qualify
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines monad transformer which qualify uniquely SQL table forms.
module Database.Relational.Query.Monad.Trans.Qualify (
  -- * Qualify monad
  Qualify, qualify,
  evalQualifyPrime, qualifyQuery
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, runStateT, get, modify)
import Control.Applicative (Applicative)
import Control.Monad (liftM)

import Database.Relational.Query.Sub (Qualified)
import qualified Database.Relational.Query.Internal.Sub as Internal


type AliasId = Int

-- | Monad type to qualify SQL table forms.
newtype Qualify m a =
  Qualify (StateT AliasId m a)
  deriving (Monad, Functor, Applicative)

-- | Run qualify monad with initial state to get only result.
evalQualifyPrime :: Monad m => Qualify m a -> m a
evalQualifyPrime (Qualify s) = fst `liftM` runStateT s 0 {- primary alias id -}

-- | Generated new qualifier on internal state.
newAlias :: Monad m => Qualify m AliasId
newAlias =  Qualify $ do
  ai <- get
  modify (+ 1)
  return ai

-- | Lift to 'Qualify'
qualify :: Monad m => m a -> Qualify m a
qualify =  Qualify . lift

-- | Get qualifyed table form query.
qualifyQuery :: Monad m
             => query                       -- ^ Query to qualify
             -> Qualify m (Qualified query) -- ^ Result with updated state
qualifyQuery query =
  do n <- newAlias
     return $ Internal.qualify (Internal.Qualifier n) query

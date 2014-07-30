{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Database.Relational.Query.Monad.Qualify
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines monad structure to qualify uniquely SQL table forms.
module Database.Relational.Query.Monad.Qualify (
  -- * Qualify monad
  Qualify,
  evalQualifyPrime, qualifyQuery
  ) where

import Control.Monad.Trans.State (State, runState, get, modify)
import Control.Applicative (Applicative)

import Database.Relational.Query.Sub (Qualified)
import qualified Database.Relational.Query.Sub as SubQuery


type AliasId = Int

-- | Monad type to qualify SQL table forms.
newtype Qualify a =
  Qualify (State AliasId a)
  deriving (Monad, Functor, Applicative)

-- | Run qualify monad with initial state to get only result.
evalQualifyPrime :: Qualify a -> a
evalQualifyPrime (Qualify s) = fst $ runState s 0 {- primary alias id -}

-- | Generated new qualifier on internal state.
newAlias :: Qualify AliasId
newAlias =  Qualify $ do
  ai <- get
  modify (+ 1)
  return ai

-- | Get qualifyed table form query.
qualifyQuery :: query                     -- ^ Query to qualify
             -> Qualify (Qualified query) -- ^ Result with updated state
qualifyQuery query =
  do n <- newAlias
     return . SubQuery.qualify query $ SubQuery.Qualifier n

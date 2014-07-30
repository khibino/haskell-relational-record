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

import Database.Relational.Query.Internal.AliasId (primeAlias, AliasId, newAliasId)
import qualified Database.Relational.Query.Internal.AliasId as AliasId
import Database.Relational.Query.Sub (Qualified)
import qualified Database.Relational.Query.Sub as SubQuery


-- | Monad type to qualify SQL table forms.
newtype Qualify a =
  Qualify { runQualify' :: State AliasId a }
  deriving (Monad, Functor, Applicative)

-- | Run qualify monad.
runQualify :: Qualify a -> AliasId -> (a, AliasId)
runQualify =  runState . runQualify'

-- | Run qualify monad with initial state.
runQualifyPrime :: Qualify a -> (a, AliasId)
runQualifyPrime q = runQualify q primeAlias

-- | Run qualify monad with initial state to get only result.
evalQualifyPrime :: Qualify a -> a
evalQualifyPrime =  fst . runQualifyPrime

-- | Generated new qualifier on internal state.
newAlias :: Qualify AliasId
newAlias =  Qualify $ do
  ai <- get
  modify newAliasId
  return ai

unsafeQualifierFromAliasId :: AliasId -> SubQuery.Qualifier
unsafeQualifierFromAliasId =  SubQuery.Qualifier . AliasId.unsafeExtractAliasId

-- | Get qualifyed table form query.
qualifyQuery :: query                     -- ^ Query to qualify
             -> Qualify (Qualified query) -- ^ Result with updated state
qualifyQuery query =
  do n <- newAlias
     return . SubQuery.qualify query $ unsafeQualifierFromAliasId n

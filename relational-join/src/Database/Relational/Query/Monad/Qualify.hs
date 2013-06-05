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

import Control.Monad (liftM, ap)
import Control.Monad.Trans.State (State, state, runState)
import Control.Applicative (Applicative (pure, (<*>)))

import Database.Relational.Query.Internal.AliasId (primeAlias, AliasId, newAliasId)
import qualified Database.Relational.Query.Internal.AliasId as AliasId
import Database.Relational.Query.Sub (Qualified)
import qualified Database.Relational.Query.Sub as SubQuery


-- | Type for 'Qualify' monad state.
newtype AliasIdContext = AliasIdContext { currentAliasId :: AliasId }

-- | Initial state.
primeAliasIdContext :: AliasIdContext
primeAliasIdContext =  AliasIdContext primeAlias

-- | Update state function.
nextAlias :: AliasIdContext -> (AliasId, AliasIdContext)
nextAlias s = (cur, s { currentAliasId =  newAliasId cur })  where
  cur = currentAliasId s


-- | Monad type to qualify SQL table forms.
newtype Qualify a =
  Qualify { runQualify' :: State AliasIdContext a }

-- | Run qualify monad.
runQualify :: Qualify a -> AliasIdContext -> (a, AliasIdContext)
runQualify =  runState . runQualify'

-- | Run qualify monad with initial state.
runQualifyPrime :: Qualify a -> (a, AliasIdContext)
runQualifyPrime q = runQualify q primeAliasIdContext

-- | Run qualify monad with initial state to get only result.
evalQualifyPrime :: Qualify a -> a
evalQualifyPrime =  fst . runQualifyPrime

-- | Make qualify monad from update state function.
qualifyState :: (AliasIdContext -> (a, AliasIdContext)) -> Qualify a
qualifyState =  Qualify . state

-- | 'Monad' instance to qualify uniquely SQL table form queries.
instance Monad Qualify where
  return      = Qualify . return
  q0 >>= f    = Qualify $ runQualify' q0 >>= runQualify' . f

-- | Define 'Functor' instance using 'Monad' methods
instance Functor Qualify where
  fmap = liftM

-- | Define 'Applicative' instance using 'Monad' methods
instance Applicative Qualify where
  pure  = return
  (<*>) = ap

-- | Generated new qualifier on internal state.
newAlias :: Qualify AliasId
newAlias =  qualifyState nextAlias

unsafeQualifierFromAliasId :: AliasId -> SubQuery.Qualifier
unsafeQualifierFromAliasId =  SubQuery.Qualifier . AliasId.unsafeExtractAliasId

-- | Get qualifyed table form query.
qualifyQuery :: query                     -- ^ Query to qualify
             -> Qualify (Qualified query) -- ^ Result with updated state
qualifyQuery query =
  do n <- newAlias
     return . SubQuery.qualify query $ unsafeQualifierFromAliasId n

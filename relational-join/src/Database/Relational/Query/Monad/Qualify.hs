{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Relational.Query.Monad.Qualify (
  Qualify, evalQualifyPrime, newAlias, qualifyQuery
  ) where

import Control.Monad (liftM, ap)
import Control.Monad.Trans.State
  (State, state, runState)
import Control.Applicative (Applicative (pure, (<*>)))

import Database.Relational.Query.Internal.AliasId (AliasId, Qualified)
import qualified Database.Relational.Query.Internal.AliasId as AliasId
import Database.Relational.Query.Internal.Context
  (AliasIdContext, primeAliasIdContext, nextAlias)


newtype Qualify a =
  Qualify { runQualify' :: State AliasIdContext a }

runQualify :: Qualify a -> AliasIdContext -> (a, AliasIdContext)
runQualify =  runState . runQualify'

runQualifyPrime :: Qualify a -> (a, AliasIdContext)
runQualifyPrime q = runQualify q primeAliasIdContext

evalQualifyPrime :: Qualify a -> a
evalQualifyPrime =  fst . runQualifyPrime

qualifyState :: (AliasIdContext -> (a, AliasIdContext)) -> Qualify a
qualifyState =  Qualify . state

instance Monad Qualify where
  return      = Qualify . return
  q0 >>= f    = Qualify $ runQualify' q0 >>= runQualify' . f

instance Functor Qualify where
  fmap = liftM

instance Applicative Qualify where
  pure  = return
  (<*>) = ap

newAlias :: Qualify AliasId
newAlias =  qualifyState nextAlias

qualifyQuery :: query -> Qualify (Qualified query)
qualifyQuery query =
  do n <- newAlias
     return $ AliasId.qualify query n

-- qualifyQuery :: Qualify query -> Qualify (Qualified query)
-- qualifyQuery qualSub = do
--   sub  <- qualSub
--   qualifyQuery' sub

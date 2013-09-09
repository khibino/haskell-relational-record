{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Query.Monad.Trans.Restricting
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines monad transformer which lift to basic 'MonadQuery'.
module Database.Relational.Query.Monad.Trans.Restricting (
  -- * Transformer into restricted context
  Restrictings, restrictings,

  -- * Result
  extractRestrict
  ) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State (modify, StateT, runStateT)
import Control.Applicative (Applicative, (<$>))
import Control.Arrow (second)

import Database.Relational.Query.Monad.Trans.RestrictingState
  (RestrictContext, primeRestrictContext, addRestriction, restriction)
import Database.Relational.Query.Expr (Expr)
import Database.Relational.Query.Component (QueryRestriction)

import Database.Relational.Query.Monad.Class (MonadRestrict(..), MonadQuery (..), MonadAggregate(..))


-- | 'StateT' type to accumulate join product context.
newtype Restrictings c m a =
  Restrictings { queryState :: StateT (RestrictContext c) m a }
  deriving (MonadTrans, Monad, Functor, Applicative)

-- | Run 'Restrictings' to expand context state.
runRestrictings :: Restrictings c m a       -- ^ RestrictContext to expand
                -> RestrictContext c        -- ^ Initial context
                -> m (a, RestrictContext c) -- ^ Expanded result
runRestrictings =  runStateT . queryState

-- | Run 'Restrictings' with primary empty context to expand context state.
runRestrictingsPrime :: Restrictings c m a       -- ^ RestrictContext to expand
                     -> m (a, RestrictContext c) -- ^ Expanded result
runRestrictingsPrime q = runRestrictings q primeRestrictContext

-- | Lift to 'Restrictings'
restrictings :: Monad m => m a -> Restrictings c m a
restrictings =  lift

-- | Unsafely update join product context.
updateRestrictContext :: Monad m => (RestrictContext c -> RestrictContext c) -> Restrictings c m ()
updateRestrictContext =  Restrictings . modify

-- | Add whole query restriction.
updateRestriction :: Monad m => Expr c (Maybe Bool) -> Restrictings c m ()
updateRestriction e = updateRestrictContext (addRestriction e)

-- | 'MonadRestrict' instance.
instance (Monad q, Functor q) => MonadRestrict c (Restrictings c q) where
  restrictContext = updateRestriction

-- | Restricted 'MonadQuery' instance.
instance MonadQuery q => MonadQuery (Restrictings c q) where
  restrictJoin     = restrictings . restrictJoin
  unsafeSubQuery a = restrictings . unsafeSubQuery a

-- | Resticted 'MonadAggregate' instance.
instance MonadAggregate m => MonadAggregate (Restrictings c m) where
  aggregateKey = restrictings . aggregateKey

-- | Run 'Restrictings' to get 'QueryRestriction'
extractRestrict :: (Monad m, Functor m) => Restrictings c m a -> m (a, QueryRestriction c)
extractRestrict q = second restriction <$> runRestrictingsPrime q

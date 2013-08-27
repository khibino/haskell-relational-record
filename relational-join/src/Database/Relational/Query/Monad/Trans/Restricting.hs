{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

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

  -- * Result SQL wheres clause
  extractWheres,
  WherePrepend, prependWhere
  ) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State (modify, StateT, runStateT)
import Control.Applicative (Applicative, (<$>))
import Control.Arrow (second)

import Database.Relational.Query.Context (Flat)
import Database.Relational.Query.Monad.Trans.StatePrepend (Prepend, prepend, liftToString)
import Database.Relational.Query.Monad.Trans.RestrictingState
  (RestrictContext, primeRestrictContext, addRestriction, composeWheres)
import Database.Relational.Query.Expr (Expr)

import Database.Relational.Query.Monad.Class (MonadRestrict(..), MonadQuery (..))


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
instance (Monad q, Functor q) => MonadRestrict (Restrictings Flat q) where
  restrictContext = updateRestriction

-- | Restricted 'MonadQuery' instance.
instance MonadQuery q => MonadQuery (Restrictings c q) where
  restrictJoin     = restrictings . restrictJoin
  unsafeSubQuery a = restrictings . unsafeSubQuery a

-- | WHERE clause prepending function.
type WherePrepend = Prepend (RestrictContext Flat)

-- | Run 'Restrictings' to get WHERE clause prepending function.
extractWheres :: (Monad m, Functor m)
              => Restrictings Flat m a -- ^ 'Restrictings' to run
              -> m (a,  WherePrepend)  -- ^ WHERE clause prepending function.
extractWheres r = second (liftToString composeWheres) <$> runRestrictingsPrime r

-- | Run WHERE clause prepend.
prependWhere :: WherePrepend -> String -> String
prependWhere =  prepend

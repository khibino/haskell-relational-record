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
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Control.Applicative (Applicative, pure, (<$>))
import Control.Arrow (second)
import Data.DList (DList, toList)

import Database.Relational.Query.Expr (Expr, fromJust)
import Database.Relational.Query.Component (QueryRestriction)

import Database.Relational.Query.Monad.Class (MonadRestrict(..), MonadQuery (..), MonadAggregate(..))


-- | 'StateT' type to accumulate join product context.
newtype Restrictings c m a =
  Restrictings (WriterT (DList (Expr c Bool)) m a)
  deriving (MonadTrans, Monad, Functor, Applicative)

-- | Lift to 'Restrictings'
restrictings :: Monad m => m a -> Restrictings c m a
restrictings =  lift

-- | Add whole query restriction.
updateRestriction :: Monad m => Expr c (Maybe Bool) -> Restrictings c m ()
updateRestriction =  Restrictings . tell . pure . fromJust

-- | 'MonadRestrict' instance.
instance (Monad q, Functor q) => MonadRestrict c (Restrictings c q) where
  restrictContext = updateRestriction

-- | Restricted 'MonadQuery' instance.
instance MonadQuery q => MonadQuery (Restrictings c q) where
  specifyDuplication = restrictings . specifyDuplication
  restrictJoin       = restrictings . restrictJoin
  unsafeSubQuery a   = restrictings . unsafeSubQuery a

-- | Resticted 'MonadAggregate' instance.
instance MonadAggregate m => MonadAggregate (Restrictings c m) where
  unsafeAddAggregateElement = restrictings . unsafeAddAggregateElement

-- | Run 'Restrictings' to get 'QueryRestriction'
extractRestrict :: (Monad m, Functor m) => Restrictings c m a -> m (a, QueryRestriction c)
extractRestrict (Restrictings rc) = second toList <$> runWriterT rc

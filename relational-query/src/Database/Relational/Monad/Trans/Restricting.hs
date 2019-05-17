{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Monad.Trans.Restricting
-- Copyright   : 2014-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines monad transformer which lift to basic 'MonadQuery'.
module Database.Relational.Monad.Trans.Restricting (
  -- * Transformer into restricted context
  Restrictings, restrictings,

  -- * Result
  extractRestrict
  ) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Control.Applicative (Applicative, (<$>))
import Control.Arrow (second)
import Data.DList (DList, toList, singleton)

import Database.Relational.SqlSyntax (Predicate, Tuple, WithPlaceholderOffsets, untypeRecordWithPlaceholderOffsets)

import Database.Relational.Monad.Class
  (MonadQualify (..), MonadRestrict(..), MonadQuery (..), MonadAggregate(..))


-- | Type to accumulate query restrictions.
--   Type 'c' is context tag of restriction building like
--   Flat (where) or Aggregated (having).
newtype Restrictings c m a =
  Restrictings (WriterT (DList (WithPlaceholderOffsets Tuple)) m a)
  deriving (Monad, Functor, Applicative)

instance MonadTrans (Restrictings c) where
  lift = Restrictings . lift

-- | Lift to 'Restrictings'
restrictings :: Monad m => m a -> Restrictings c m a
restrictings =  lift

-- | Add whole query restriction.
updateRestriction :: Monad m => Predicate c -> Restrictings c m ()
updateRestriction = Restrictings . tell . singleton . untypeRecordWithPlaceholderOffsets

-- | 'MonadRestrict' instance.
instance (Monad q, Functor q) => MonadRestrict c (Restrictings c q) where
  restrict = updateRestriction

-- | Restricted 'MonadQualify' instance.
instance MonadQualify q m => MonadQualify q (Restrictings c m) where
  liftQualify = restrictings . liftQualify

-- | Restricted 'MonadQuery' instance.
instance MonadQuery q => MonadQuery (Restrictings c q) where
  setDuplication     = restrictings . setDuplication
  restrictJoin       = restrictings . restrictJoin
  query' ph          = restrictings . query' ph
  queryMaybe' ph     = restrictings . queryMaybe' ph

-- | Resticted 'MonadAggregate' instance.
instance MonadAggregate m => MonadAggregate (Restrictings c m) where
  groupBy  = restrictings . groupBy
  groupBy' = restrictings . groupBy'

-- | Run 'Restrictings' to get 'QueryRestriction'
extractRestrict :: (Monad m, Functor m) => Restrictings c m a -> m (a, [WithPlaceholderOffsets Tuple])
extractRestrict (Restrictings rc) = second toList <$> runWriterT rc

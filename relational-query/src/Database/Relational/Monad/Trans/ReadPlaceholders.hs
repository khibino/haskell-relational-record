{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Monad.Trans.Config
-- Copyright   : 2019 IIJ Innovation Institute Inc.
-- License     : BSD3
--
-- Maintainer  : yuji-yamamoto@iij.ad.jp
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines monad transformer which requires query generate configuration.
module Database.Relational.Monad.Trans.ReadPlaceholders (
  module Database.Relational.Monad.Trans.ReadPlaceholders.Type
  ) where

import Database.Relational.Monad.Trans.ReadPlaceholders.Type
import Database.Relational.Monad.Class
  (MonadQualify (..), MonadRestrict(..), MonadQuery (..), MonadAggregate(..))

-- | 'MonadRestrict' instance.
instance MonadRestrict c m => MonadRestrict c (ReadPlaceholders p m) where
  restrict =  readPlaceholders . restrict

-- | Restricted 'MonadQualify' instance.
instance MonadQualify q m => MonadQualify q (ReadPlaceholders p m) where
  liftQualify = readPlaceholders . liftQualify

-- | Restricted 'MonadQuery' instance.
instance MonadQuery q => MonadQuery (ReadPlaceholders p q) where
  setDuplication     = readPlaceholders . setDuplication
  restrictJoin       = readPlaceholders . restrictJoin
  query' ph          = readPlaceholders . query' ph
  queryMaybe' ph     = readPlaceholders . queryMaybe' ph

-- | Resticted 'MonadAggregate' instance.
instance MonadAggregate m => MonadAggregate (ReadPlaceholders p m) where
  groupBy  = readPlaceholders . groupBy
  groupBy' = readPlaceholders . groupBy'

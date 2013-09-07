{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Database.Relational.Query.Monad.Trans.Config
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines monad transformer which requires query generate configuration.
module Database.Relational.Query.Monad.Trans.Config (
  -- * Transformer into query with configuration
  QueryConfig, config,
  runQueryConfig, askConfig
  ) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Applicative (Applicative)

import Database.Relational.Query.Sub (Config)


-- | 'ReaderT' type to require query generate configuration.
newtype QueryConfig m a =
  QueryConfig { queryConfig :: ReaderT Config m a }
  deriving (MonadTrans, Monad, Functor, Applicative)

-- | Run 'QueryConfig' to expand with configuration
runQueryConfig :: QueryConfig m a -> Config -> m a
runQueryConfig =  runReaderT . queryConfig

-- | Lift to 'QueryConfig'.
config :: Monad m => m a -> QueryConfig m a
config =  lift

-- | Read configuration.
askConfig :: Monad m => QueryConfig m Config
askConfig =  QueryConfig $ ask

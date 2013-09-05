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
  defaultConfig,

  QueryConfig, config,
  runQueryConfig, runQueryDefault
  ) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Applicative (Applicative)

import Database.Relational.Query.Sub (UnitProductSupport (UPSupported))


-- | Configuration type.
type Config = UnitProductSupport

-- | Default configuration.
defaultConfig :: Config
defaultConfig =  UPSupported

-- | 'ReaderT' type to require query generate configuration.
newtype QueryConfig m a =
  QueryConfig { queryConfig :: ReaderT Config m a }
  deriving (MonadTrans, Monad, Functor, Applicative)

-- | Run 'QueryConfig' to expand with configuration
runQueryConfig :: QueryConfig m a -> Config -> m a
runQueryConfig =  runReaderT . queryConfig

-- | Run 'QueryConfig' with defualt configuration
runQueryDefault ::  QueryConfig m a -> m a
runQueryDefault q = runQueryConfig q defaultConfig

-- | Lift to 'QueryConfig'
config :: Monad m => m a -> QueryConfig m a
config =  lift

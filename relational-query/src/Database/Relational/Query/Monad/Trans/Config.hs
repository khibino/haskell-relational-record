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
  QueryConfig, queryConfig,
  runQueryConfig, askQueryConfig
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Applicative (Applicative)

import Database.Relational.Query.Component (Config)


-- | 'ReaderT' type to require query generate configuration.
newtype QueryConfig m a =
  QueryConfig (ReaderT Config m a)
  deriving (Monad, Functor, Applicative)

-- | Run 'QueryConfig' to expand with configuration
runQueryConfig :: QueryConfig m a -> Config -> m a
runQueryConfig (QueryConfig r) = runReaderT r

-- | Lift to 'QueryConfig'.
queryConfig :: Monad m => m a -> QueryConfig m a
queryConfig =  QueryConfig . lift

-- | Read configuration.
askQueryConfig :: Monad m => QueryConfig m Config
askQueryConfig =  QueryConfig ask

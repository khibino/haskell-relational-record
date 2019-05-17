{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
module Database.Relational.Monad.Trans.ReadPlaceholders.Type (
  -- * Transformer into query with configuration
  ReadPlaceholders, readPlaceholders,
  runReadPlaceholders, askPlaceholders,
  ) where

import Control.Applicative (Applicative)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)

import Database.Relational.Internal.ContextType (PureOperand)
import Database.Relational.SqlSyntax (Record)


-- | 'ReaderT' type to require query generate configuration.
newtype ReadPlaceholders p m a =
  ReadPlaceholders (ReaderT (Record PureOperand p) m a)
  deriving (Monad, Functor, Applicative)

-- | Run 'ReadPlaceholders' to expand with configuration
runReadPlaceholders :: ReadPlaceholders p m a -> Record PureOperand p -> m a
runReadPlaceholders (ReadPlaceholders r) = runReaderT r

-- | Lift to 'ReadPlaceholders'.
readPlaceholders :: Monad m => m a -> ReadPlaceholders p m a
readPlaceholders =  ReadPlaceholders . lift

-- | Read configuration.
askPlaceholders :: Monad m => ReadPlaceholders p m (Record PureOperand p)
askPlaceholders =  ReadPlaceholders ask

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Database.Relational.PureUTF8
-- Copyright   : 2013-2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines instances to lift from haskell UTF8 byte-sequence
-- to query internal record values.
-- This module is not defaultly imported to be selectable instance of byte-sequences.
module Database.Relational.PureUTF8 () where

import Control.Applicative (pure)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT

import qualified Database.Relational.Internal.Literal as Lit

import Database.Relational.ProjectableClass (LiteralSQL (..))


-- | Constant SQL terms of 'ByteString'.
instance LiteralSQL ByteString where
  showLiteral' = pure . Lit.stringExpr . T.unpack . T.decodeUtf8

-- | Constant SQL terms of 'LB.ByteString'.
instance LiteralSQL LB.ByteString where
  showLiteral' = pure . Lit.stringExpr . LT.unpack . LT.decodeUtf8

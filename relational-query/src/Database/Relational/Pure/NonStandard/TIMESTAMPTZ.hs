{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Relational.Pure.NonStandard.TIMESTAMPTZ () where

import Control.Applicative (pure)
import Data.Time (UTCTime, ZonedTime)

import Language.SQL.Keyword (Keyword (..))

import qualified Database.Relational.Internal.Literal as Lit

import Database.Relational.ProjectableClass (LiteralSQL (..))


-- | Constant SQL terms of 'ZonedTime'.
--   This generates ***NOT STANDARD*** SQL of TIMESTAMPTZ literal.
instance LiteralSQL ZonedTime where
  showLiteral' = pure . Lit.timestamp TIMESTAMPTZ "%Y-%m-%d %H:%M:%S%z"

-- | Constant SQL terms of 'UTCTime'.
--   This generates ***NOT STANDARD*** SQL of TIMESTAMPTZ literal with UTC timezone.
instance LiteralSQL UTCTime where
  showLiteral' = pure . Lit.timestamp TIMESTAMPTZ "%Y-%m-%d %H:%M:%S%z"

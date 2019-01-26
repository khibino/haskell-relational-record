{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Relational.Pure.NonStandard.TIMESTAMPTZ () where

import Data.Time (UTCTime, ZonedTime)
import Database.Relational.ProjectableClass (LiteralSQL (..))
import Database.Relational.Pure.Internal (constantTimeTerms)
import Language.SQL.Keyword (Keyword (..))

-- | Constant SQL terms of 'ZonedTime'.
--   This generates ***NOT STANDARD*** SQL of TIMESTAMPTZ literal.
instance LiteralSQL ZonedTime where
  showLiteral' = constantTimeTerms TIMESTAMPTZ "%Y-%m-%d %H:%M:%S%z"

-- | Constant SQL terms of 'UTCTime'.
--   This generates ***NOT STANDARD*** SQL of TIMESTAMPTZ literal with UTC timezone.
instance LiteralSQL UTCTime where
  showLiteral' = constantTimeTerms TIMESTAMPTZ "%Y-%m-%d %H:%M:%S%z"


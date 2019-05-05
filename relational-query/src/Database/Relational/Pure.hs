{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Database.Relational.Pure
-- Copyright   : 2013-2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines instances which lift from haskell pure values
-- to query internal record values.
module Database.Relational.Pure () where

import Control.Applicative (pure)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64, Word)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Text.Printf (PrintfArg, printf)
import Data.Time (Day, TimeOfDay, LocalTime)
import Data.DList (DList, fromList)

import Language.SQL.Keyword (Keyword (..))
import Database.Record
  (PersistableWidth, persistableWidth, PersistableRecordWidth)
import Database.Record.Persistable
  (runPersistableRecordWidth)

import Database.Relational.Internal.String (StringSQL, stringSQL)
import qualified Database.Relational.Internal.Literal as Lit

import Database.Relational.ProjectableClass (LiteralSQL (..))


-- | Constant SQL terms of '()'.
instance LiteralSQL ()

-- | Constant SQL terms of 'Int8'.
instance LiteralSQL Int8 where
  showLiteral' = pure . Lit.integral

-- | Constant SQL terms of 'Int16'.
instance LiteralSQL Int16 where
  showLiteral' = pure . Lit.integral

-- | Constant SQL terms of 'Int32'.
instance LiteralSQL Int32 where
  showLiteral' = pure . Lit.integral

-- | Constant SQL terms of 'Int64'.
instance LiteralSQL Int64 where
  showLiteral' = pure . Lit.integral

-- | Constant SQL terms of 'Int'.
--   Use this carefully, because this is architecture dependent size of integer type.
instance LiteralSQL Int where
  showLiteral' = pure . Lit.integral

-- | Constant SQL terms of 'Word8'.
instance LiteralSQL Word8 where
  showLiteral' = pure . Lit.integral

-- | Constant SQL terms of 'Word16'.
instance LiteralSQL Word16 where
  showLiteral' = pure . Lit.integral

-- | Constant SQL terms of 'Word32'.
instance LiteralSQL Word32 where
  showLiteral' = pure . Lit.integral

-- | Constant SQL terms of 'Word64'.
instance LiteralSQL Word64 where
  showLiteral' = pure . Lit.integral

-- | Constant SQL terms of 'Word'.
--   Use this carefully, because this is architecture dependent size of integer type.
instance LiteralSQL Word where
  showLiteral' = pure . Lit.integral

instance LiteralSQL Integer where
  showLiteral' = pure . Lit.integral

-- | Constant SQL terms of 'String'.
instance LiteralSQL String where
  showLiteral' = pure . Lit.stringExpr

-- | Constant SQL terms of 'Text'.
instance LiteralSQL Text where
  showLiteral' = pure . Lit.stringExpr . T.unpack

-- | Constant SQL terms of 'LT.Text'.
instance LiteralSQL LT.Text where
  showLiteral' = pure . Lit.stringExpr . LT.unpack

-- | Constant SQL terms of 'Char'.
instance LiteralSQL Char where
  showLiteral' = pure . Lit.stringExpr . (:"")

-- | Constant SQL terms of 'Bool'.
instance LiteralSQL Bool where
  showLiteral' = pure . Lit.bool

floatTerms :: (PrintfArg a, Ord a, Num a)=> a -> DList StringSQL
floatTerms f = pure . stringSQL $ printf fmt f  where
  fmt
    | f >= 0    = "%f"
    | otherwise = "(%f)"

-- | Constant SQL terms of 'Float'. Caution for floating-point error rate.
instance LiteralSQL Float where
  showLiteral' = floatTerms

-- | Constant SQL terms of 'Double'. Caution for floating-point error rate.
instance LiteralSQL Double where
  showLiteral' = floatTerms

-- | Constant SQL terms of 'Day'.
instance LiteralSQL Day where
  showLiteral' = pure . Lit.timestamp DATE "%Y-%m-%d"

-- | Constant SQL terms of 'TimeOfDay'.
instance LiteralSQL TimeOfDay where
  showLiteral' = pure . Lit.timestamp TIME "%H:%M:%S"

-- | Constant SQL terms of 'LocalTime'.
instance LiteralSQL LocalTime where
  showLiteral' = pure . Lit.timestamp TIMESTAMP "%Y-%m-%d %H:%M:%S"

showMaybeTerms :: LiteralSQL a => PersistableRecordWidth a -> Maybe a -> DList StringSQL
showMaybeTerms wa = d  where
  d (Just a) = showLiteral' a
  d Nothing  = fromList . replicate (runPersistableRecordWidth wa) $ stringSQL "NULL"

-- | Constant SQL terms of 'Maybe' type. Width inference is required.
instance (PersistableWidth a, LiteralSQL a)
         => LiteralSQL (Maybe a) where
  showLiteral' = showMaybeTerms persistableWidth

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Database.Relational.Pure
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines interfaces between haskell pure values
-- and query internal projection values.
module Database.Relational.Pure () where

import Control.Applicative (pure)
import Data.Monoid ((<>))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Text.Printf (PrintfArg, printf)
import Data.Time (FormatTime, Day, TimeOfDay, LocalTime, UTCTime, ZonedTime, formatTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Data.DList (DList, fromList)

import Language.SQL.Keyword (Keyword (..))
import Database.Record
  (PersistableWidth, persistableWidth, PersistableRecordWidth)
import Database.Record.Persistable
  (runPersistableRecordWidth)

import Database.Relational.Internal.SQL (StringSQL, stringSQL)

import Database.Relational.ProjectableClass (ShowConstantTermsSQL (..))


-- | Constant integral SQL terms.
intTermsSQL :: (Show a, Integral a) => a -> DList StringSQL
intTermsSQL =  pure . stringSQL . show

-- | Escape 'String' for constant SQL string expression.
escapeStringToSqlExpr :: String -> String
escapeStringToSqlExpr =  rec  where
  rec ""        = ""
  rec ('\'':cs) = '\'' : '\'' : rec cs
  rec (c:cs)    = c : rec cs

-- | From 'String' into constant SQL string expression.
stringExprSQL :: String -> StringSQL
stringExprSQL =  stringSQL . ('\'':) . (++ "'") . escapeStringToSqlExpr

stringTermsSQL :: String -> DList StringSQL
stringTermsSQL = pure . stringExprSQL

-- | Constant SQL terms of '()'.
instance ShowConstantTermsSQL ()

-- | Constant SQL terms of 'Int8'.
instance ShowConstantTermsSQL Int8 where
  showConstantTermsSQL' = intTermsSQL

-- | Constant SQL terms of 'Int16'.
instance ShowConstantTermsSQL Int16 where
  showConstantTermsSQL' = intTermsSQL

-- | Constant SQL terms of 'Int32'.
instance ShowConstantTermsSQL Int32 where
  showConstantTermsSQL' = intTermsSQL

-- | Constant SQL terms of 'Int64'.
instance ShowConstantTermsSQL Int64 where
  showConstantTermsSQL' = intTermsSQL

-- | Constant SQL terms of 'Int'.
--   Use this carefully, because this is architecture dependent size of integer type.
instance ShowConstantTermsSQL Int where
  showConstantTermsSQL' = intTermsSQL

-- | Constant SQL terms of 'String'.
instance ShowConstantTermsSQL String where
  showConstantTermsSQL' = stringTermsSQL

-- | Constant SQL terms of 'ByteString'.
instance ShowConstantTermsSQL ByteString where
  showConstantTermsSQL' = stringTermsSQL . T.unpack . T.decodeUtf8

-- | Constant SQL terms of 'LB.ByteString'.
instance ShowConstantTermsSQL LB.ByteString where
  showConstantTermsSQL' = stringTermsSQL . LT.unpack . LT.decodeUtf8

-- | Constant SQL terms of 'Text'.
instance ShowConstantTermsSQL Text where
  showConstantTermsSQL' = stringTermsSQL . T.unpack

-- | Constant SQL terms of 'LT.Text'.
instance ShowConstantTermsSQL LT.Text where
  showConstantTermsSQL' = stringTermsSQL . LT.unpack

-- | Constant SQL terms of 'Char'.
instance ShowConstantTermsSQL Char where
  showConstantTermsSQL' = stringTermsSQL . (:"")

-- | Constant SQL terms of 'Bool'.
instance ShowConstantTermsSQL Bool where
  showConstantTermsSQL' = pure . stringSQL . d  where
    d True  = "(0=0)"
    d False = "(0=1)"

floatTerms :: (PrintfArg a, Ord a, Num a)=> a -> DList StringSQL
floatTerms f = pure . stringSQL $ printf fmt f  where
  fmt
    | f >= 0    = "%f"
    | otherwise = "(%f)"

-- | Constant SQL terms of 'Float'. Caution for floating-point error rate.
instance ShowConstantTermsSQL Float where
  showConstantTermsSQL' = floatTerms

-- | Constant SQL terms of 'Double'. Caution for floating-point error rate.
instance ShowConstantTermsSQL Double where
  showConstantTermsSQL' = floatTerms

constantTimeTerms :: FormatTime t => Keyword -> String -> t -> DList StringSQL
constantTimeTerms kw fmt t = pure $ kw <> stringExprSQL (formatTime defaultTimeLocale fmt t)

-- | Constant SQL terms of 'Day'.
instance ShowConstantTermsSQL Day where
  showConstantTermsSQL' = constantTimeTerms DATE "%Y-%m-%d"

-- | Constant SQL terms of 'TimeOfDay'.
instance ShowConstantTermsSQL TimeOfDay where
  showConstantTermsSQL' = constantTimeTerms TIME "%H:%M:%S"

-- | Constant SQL terms of 'LocalTime'.
instance ShowConstantTermsSQL LocalTime where
  showConstantTermsSQL' = constantTimeTerms TIMESTAMP "%Y-%m-%d %H:%M:%S"

-- | Constant SQL terms of 'ZonedTime'.
--   This generates ***NOT STANDARD*** SQL of TIMESTAMPTZ literal.
instance ShowConstantTermsSQL ZonedTime where
  showConstantTermsSQL' = constantTimeTerms TIMESTAMPTZ "%Y-%m-%d %H:%M:%S%z"

-- | Constant SQL terms of 'UTCTime'.
--   This generates ***NOT STANDARD*** SQL of TIMESTAMPTZ literal with UTC timezone.
instance ShowConstantTermsSQL UTCTime where
  showConstantTermsSQL' = constantTimeTerms TIMESTAMPTZ "%Y-%m-%d %H:%M:%S%z"

showMaybeTerms :: ShowConstantTermsSQL a => PersistableRecordWidth a -> Maybe a -> DList StringSQL
showMaybeTerms wa = d  where
  d (Just a) = showConstantTermsSQL' a
  d Nothing  = fromList . replicate (runPersistableRecordWidth wa) $ stringSQL "NULL"

-- | Constant SQL terms of 'Maybe' type. Width inference is required.
instance (PersistableWidth a, ShowConstantTermsSQL a)
         => ShowConstantTermsSQL (Maybe a) where
  showConstantTermsSQL' = showMaybeTerms persistableWidth

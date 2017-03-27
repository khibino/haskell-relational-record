{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Database.Relational.Query.Pure
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines interfaces between haskell pure values
-- and query internal projection values.
module Database.Relational.Query.Pure (

  -- * Interface to specify record constructors.
  ProductConstructor (..),

  -- * Constant SQL Terms
  ShowConstantTermsSQL (..), showConstantTermsSQL
  ) where

import Data.Monoid (mconcat)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import Text.Printf (PrintfArg, printf)
import Data.Time (FormatTime, Day, TimeOfDay, LocalTime, UTCTime, ZonedTime, formatTime)
import Data.Time.Locale.Compat (defaultTimeLocale)

import Language.SQL.Keyword (Keyword (..))
import Database.Record
  (PersistableWidth, persistableWidth, PersistableRecordWidth)
import Database.Record.Persistable
  (runPersistableRecordWidth)

import Database.Relational.Query.Internal.SQL (StringSQL, stringSQL, showStringSQL)
import Database.Relational.Query.Internal.ProjectableClass (ProductConstructor (..))


-- | ProductConstructor instance of pair.
instance ProductConstructor (a -> b -> (a, b)) where
  productConstructor = (,)


-- | Constant integral SQL expression.
intExprSQL :: (Show a, Integral a) => a -> StringSQL
intExprSQL =  stringSQL . show

intTermsSQL :: (Show a, Integral a) => a -> [StringSQL]
intTermsSQL =  (:[]) . intExprSQL

-- | Escape 'String' for constant SQL string expression.
escapeStringToSqlExpr :: String -> String
escapeStringToSqlExpr =  rec  where
  rec ""        = ""
  rec ('\'':cs) = '\'' : '\'' : rec cs
  rec (c:cs)    = c : rec cs

-- | From 'String' into constant SQL string expression.
stringExprSQL :: String -> StringSQL
stringExprSQL =  stringSQL . ('\'':) . (++ "'") . escapeStringToSqlExpr

stringTermsSQL :: String -> [StringSQL]
stringTermsSQL =  (:[]) . stringExprSQL

-- | Interface for constant SQL term list.
class ShowConstantTermsSQL a where
  showConstantTermsSQL' :: a -> [StringSQL]

-- | String interface of 'showConstantTermsSQL''.
showConstantTermsSQL :: ShowConstantTermsSQL a => a -> [String]
showConstantTermsSQL =  map showStringSQL . showConstantTermsSQL'

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
  showConstantTermsSQL' = mconcat . map showConstantTermsSQL' . LB.toChunks

-- | Constant SQL terms of 'Text'.
instance ShowConstantTermsSQL Text where
  showConstantTermsSQL' = stringTermsSQL . T.unpack

-- | Constant SQL terms of 'LT.Text'.
instance ShowConstantTermsSQL LT.Text where
  showConstantTermsSQL' = mconcat . map showConstantTermsSQL' . LT.toChunks

-- | Constant SQL terms of 'Char'.
instance ShowConstantTermsSQL Char where
  showConstantTermsSQL' = stringTermsSQL . (:"")

-- | Constant SQL terms of 'Bool'.
instance ShowConstantTermsSQL Bool where
  showConstantTermsSQL' = (:[]) . stringSQL . d  where
    d True  = "(0=0)"
    d False = "(0=1)"

floatTerms :: (PrintfArg a, Ord a, Num a)=> a -> [StringSQL]
floatTerms f = (:[]) . stringSQL $ printf fmt f  where
  fmt
    | f >= 0    = "%f"
    | otherwise = "(%f)"

-- | Constant SQL terms of 'Float'. Caution for floating-point error rate.
instance ShowConstantTermsSQL Float where
  showConstantTermsSQL' = floatTerms

-- | Constant SQL terms of 'Double'. Caution for floating-point error rate.
instance ShowConstantTermsSQL Double where
  showConstantTermsSQL' = floatTerms

constantTimeTerms :: FormatTime t => Keyword -> String -> t -> [StringSQL]
constantTimeTerms kw fmt t = [mconcat [kw,
                                       stringExprSQL $ formatTime defaultTimeLocale fmt t]]

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

showMaybeTerms :: ShowConstantTermsSQL a => PersistableRecordWidth a -> Maybe a -> [StringSQL]
showMaybeTerms wa = d  where
  d (Just a) = showConstantTermsSQL' a
  d Nothing  = replicate (runPersistableRecordWidth wa) $ stringSQL "NULL"

-- | Constant SQL terms of 'Maybe' type. Width inference is required.
instance (PersistableWidth a, ShowConstantTermsSQL a)
         => ShowConstantTermsSQL (Maybe a) where
  showConstantTermsSQL' = showMaybeTerms persistableWidth

-- | Constant SQL terms of '(a, b)' type.
instance (ShowConstantTermsSQL a, ShowConstantTermsSQL b)
         => ShowConstantTermsSQL (a, b) where
  showConstantTermsSQL' (a, b) = showConstantTermsSQL' a ++ showConstantTermsSQL' b

{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Database.Relational.Query.Pure
-- Copyright   : 2013 Kei Hibino
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
  ShowConstantTermsSQL (..)
  ) where

import Data.Int (Int16, Int32, Int64)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (FormatTime, Day, TimeOfDay, LocalTime, formatTime)
import System.Locale (defaultTimeLocale)

import Language.SQL.Keyword (Keyword (..), wordShow)
import Database.Record
  (PersistableWidth, persistableWidth, PersistableRecordWidth)
import Database.Record.Persistable
  (runPersistableRecordWidth)


-- | Specify tuple like record constructors which are allowed to define 'ProjectableFunctor'.
class ProductConstructor r where
  -- | The constructor which has type 'r'.
  productConstructor :: r

-- | ProductConstructor instance of pair.
instance ProductConstructor (a -> b -> (a, b)) where
  productConstructor = (,)


-- | Constant integral SQL expression.
intExprSQL :: (Show a, Integral a) => a -> String
intExprSQL =  show

intTermsSQL :: (Show a, Integral a) => a -> [String]
intTermsSQL =  (:[]) . intExprSQL

-- | Escape 'String' for constant SQL string expression.
escapeStringToSqlExpr :: String -> String
escapeStringToSqlExpr =  rec  where
  rec ""        = ""
  rec ('\'':cs) = '\'' : '\'' : rec cs
  rec (c:cs)    = c : rec cs

-- | From 'String' into constant SQL string expression.
stringExprSQL :: String -> String
stringExprSQL =  ('\'':) . (++ "'") . escapeStringToSqlExpr

stringTermsSQL :: String -> [String]
stringTermsSQL =  (:[]) . stringExprSQL

-- | Interface for constant SQL term list.
class ShowConstantTermsSQL a where
  showConstantTermsSQL :: a -> [String]

-- | Constant SQL terms of 'Int16'.
instance ShowConstantTermsSQL Int16 where
  showConstantTermsSQL = intTermsSQL

-- | Constant SQL terms of 'Int32'.
instance ShowConstantTermsSQL Int32 where
  showConstantTermsSQL = intTermsSQL

-- | Constant SQL terms of 'Int64'.
instance ShowConstantTermsSQL Int64 where
  showConstantTermsSQL = intTermsSQL

-- | Constant SQL terms of 'String'.
instance ShowConstantTermsSQL String where
  showConstantTermsSQL = stringTermsSQL

-- | Constant SQL terms of 'ByteString'.
instance ShowConstantTermsSQL ByteString where
  showConstantTermsSQL = stringTermsSQL . T.unpack . T.decodeUtf8

-- | Constant SQL terms of 'Text'.
instance ShowConstantTermsSQL Text where
  showConstantTermsSQL = stringTermsSQL . T.unpack

-- | Constant SQL terms of 'Char'.
instance ShowConstantTermsSQL Char where
  showConstantTermsSQL = stringTermsSQL . (:"")

-- | Constant SQL terms of 'Bool'.
instance ShowConstantTermsSQL Bool where
  showConstantTermsSQL = (:[]) . d  where
    d True  = "(0=0)"
    d False = "(0=1)"

constantTimeTerms :: FormatTime t => Keyword -> String -> t -> [String]
constantTimeTerms kw fmt t = [unwords [wordShow kw,
                                       stringExprSQL $ formatTime defaultTimeLocale fmt t]]

instance ShowConstantTermsSQL Day where
  showConstantTermsSQL = constantTimeTerms DATE "%Y-%m-%d"

instance ShowConstantTermsSQL TimeOfDay where
  showConstantTermsSQL = constantTimeTerms TIME "%H:%M:%S"

instance ShowConstantTermsSQL LocalTime where
  showConstantTermsSQL = constantTimeTerms TIMESTAMP "%Y-%m-%d %H:%M:%S"

showMaybeTerms :: ShowConstantTermsSQL a => PersistableRecordWidth a -> Maybe a -> [String]
showMaybeTerms wa = d  where
  d (Just a) = showConstantTermsSQL a
  d Nothing  = replicate (runPersistableRecordWidth wa) "NULL"

-- | Constant SQL terms of 'Maybe' type. Width inference is required.
instance (PersistableWidth a, ShowConstantTermsSQL a)
         => ShowConstantTermsSQL (Maybe a) where
  showConstantTermsSQL = showMaybeTerms persistableWidth

-- | Constant SQL terms of '(a, b)' type.
instance (ShowConstantTermsSQL a, ShowConstantTermsSQL b)
         => ShowConstantTermsSQL (a, b) where
  showConstantTermsSQL (a, b) = showConstantTermsSQL a ++ showConstantTermsSQL b

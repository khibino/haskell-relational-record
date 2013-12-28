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
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T

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

-- | Escape 'String' for constant SQL string expression.
escapeStringToSqlExpr :: String -> String
escapeStringToSqlExpr =  rec  where
  rec ""        = ""
  rec ('\'':cs) = '\'' : '\'' : rec cs
  rec (c:cs)    = c : rec cs

-- | From 'String' into constant SQL string expression.
stringExprSQL :: String -> String
stringExprSQL =  ('\'':) . (++ "'") . escapeStringToSqlExpr

-- | Interface for constant SQL expression.
class ShowConstantSQL a where
  -- | Make constant SQL expression 'String' from Haskell type 'a'.
  showConstantSQL :: a -> String

-- | Constant SQL expression of 'Int16'.
instance ShowConstantSQL Int16 where
  showConstantSQL = intExprSQL

-- | Constant SQL expression of 'Int32'.
instance ShowConstantSQL Int32 where
  showConstantSQL = intExprSQL

-- | Constant SQL expression of 'Int64'.
instance ShowConstantSQL Int64 where
  showConstantSQL = intExprSQL

-- | Constant SQL expression of 'String'.
instance ShowConstantSQL String where
  showConstantSQL = stringExprSQL

-- | Constant SQL expression of 'ByteString'.
instance ShowConstantSQL ByteString where
  showConstantSQL = stringExprSQL . BS.unpack

-- | Constant SQL expression of 'Text'.
instance ShowConstantSQL Text where
  showConstantSQL = stringExprSQL . T.unpack

-- | Constant SQL expression of 'Char'.
instance ShowConstantSQL Char where
  showConstantSQL = stringExprSQL . (:"")

-- | Constant SQL expression of 'Bool'.
instance ShowConstantSQL Bool where
  showConstantSQL = d  where
    d True  = "(0=0)"
    d False = "(0=1)"

-- | Inference rule for Constant SQL expression of 'Maybe' type.
instance ShowConstantSQL a => ShowConstantSQL (Maybe a) where
  showConstantSQL = d  where
    d (Just a)  = showConstantSQL a
    d (Nothing) = "NULL"


-- | Interface for constant SQL term list.
class ShowConstantTermsSQL a where
  showConstantTermsSQL :: a -> [String]

showSingleTerm :: ShowConstantSQL a => a -> [String]
showSingleTerm =  (:[]) . showConstantSQL

-- | Constant SQL terms of 'Int16'.
instance ShowConstantTermsSQL Int16 where
  showConstantTermsSQL = showSingleTerm

-- | Constant SQL terms of 'Int32'.
instance ShowConstantTermsSQL Int32 where
  showConstantTermsSQL = showSingleTerm

-- | Constant SQL terms of 'Int64'.
instance ShowConstantTermsSQL Int64 where
  showConstantTermsSQL = showSingleTerm

-- | Constant SQL terms of 'String'.
instance ShowConstantTermsSQL String where
  showConstantTermsSQL = showSingleTerm

-- | Constant SQL terms of 'ByteString'.
instance ShowConstantTermsSQL ByteString where
  showConstantTermsSQL = showSingleTerm

-- | Constant SQL terms of 'Text'.
instance ShowConstantTermsSQL Text where
  showConstantTermsSQL = showSingleTerm

-- | Constant SQL terms of 'Char'.
instance ShowConstantTermsSQL Char where
  showConstantTermsSQL = showSingleTerm

-- | Constant SQL terms of 'Bool'.
instance ShowConstantTermsSQL Bool where
  showConstantTermsSQL = showSingleTerm

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

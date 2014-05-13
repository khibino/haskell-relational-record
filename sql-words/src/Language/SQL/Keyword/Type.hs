-- |
-- Module      : Language.SQL.Keyword.Type
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- SQL keyword representation using Haskell data constructors.
module Language.SQL.Keyword.Type (
  Keyword (..),

  word,
  wordShow, unwordsSQL,

  unsafeString, integer,

  stringMap
  ) where

import Data.Monoid (mconcat)
import Language.SQL.Keyword.Internal.Type (Keyword (..), word, wordShow)


-- | Concatinate keywords into 'String' like unwords
unwordsSQL :: [Keyword] -> String
unwordsSQL =  wordShow . mconcat

-- | Make SQL string expression. No escape logic, so this is unsafe function.
unsafeString :: String -> Keyword
unsafeString =  word . ('\'' :) . (++ "'")

-- | Make SQL integer expression
integer :: (Integral a, Show a) => a -> Keyword
integer =  word . show

-- | Map 'String' function into 'Keyword' function
stringMap :: (String -> String) -> Keyword -> Keyword
stringMap f = word . f . wordShow

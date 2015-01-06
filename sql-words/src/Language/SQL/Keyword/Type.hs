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
  Keyword (..), DString,

  word,
  wordShow, unwordsSQL
  ) where

import Data.Monoid (mconcat)
import Language.SQL.Keyword.Internal.Type (Keyword (..), word, wordShow, DString)


-- | Concatinate keywords into 'String' like unwords
unwordsSQL :: [Keyword] -> String
unwordsSQL =  wordShow . mconcat

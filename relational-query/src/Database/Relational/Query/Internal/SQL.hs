-- |
-- Module      : Database.Relational.Query.Internal.SQL
-- Copyright   : 2014 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides SQL string wrap interfaces.
module Database.Relational.Query.Internal.SQL (
  StringSQL, stringSQL, showStringSQL,

  rowStringSQL, rowPlaceHolderStringSQL,

  rowListStringSQL, rowListStringString
  ) where

import Language.SQL.Keyword (Keyword, word, wordShow, fold, (|*|), paren)


-- | String wrap type for SQL strings.
type StringSQL = Keyword

-- | 'StringSQL' from 'String'.
stringSQL :: String -> StringSQL
stringSQL =  word

-- | 'StringSQL' to 'String'.
showStringSQL :: StringSQL -> String
showStringSQL =  wordShow

-- | Row String of SQL values.
rowStringSQL :: [StringSQL] -> StringSQL
rowStringSQL =  d  where
  d []  = error "Projection: no columns."
  d [c] = c
  d cs  = paren $ fold (|*|) cs

-- | Place holder row String of SQL.
rowPlaceHolderStringSQL :: Int -> StringSQL
rowPlaceHolderStringSQL =  rowStringSQL . (`replicate` stringSQL "?")

-- | Rows String of SQL.
rowListStringSQL :: [StringSQL] -> StringSQL
rowListStringSQL =  paren . fold (|*|)

-- | Rows String of SQL. String type version.
rowListStringString :: [String] -> String
rowListStringString =  wordShow . rowListStringSQL . map word

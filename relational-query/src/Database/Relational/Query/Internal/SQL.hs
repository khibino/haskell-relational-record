{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Database.Relational.Query.Internal.SQL
-- Copyright   : 2014-2017 Kei Hibino
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

  rowConsStringSQL, listStringSQL,

  ColumnSQL, columnSQL, columnSQL', showsColumnSQL,
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

-- | List String of SQL.
rowConsStringSQL :: [StringSQL] -> StringSQL
rowConsStringSQL =  paren . fold (|*|)

-- | List String of SQL.
listStringSQL :: [StringSQL] -> StringSQL
listStringSQL =  paren . fold (|*|)


-- | Simple wrap type
newtype ColumnSQL' a = ColumnSQL a

instance Functor ColumnSQL' where
  fmap f (ColumnSQL c) = ColumnSQL $ f c

-- | Column SQL string type
type ColumnSQL = ColumnSQL' StringSQL

-- | 'ColumnSQL' from string
columnSQL :: String -> ColumnSQL
columnSQL =  columnSQL' . stringSQL

-- | 'ColumnSQL' from 'StringSQL'
columnSQL' :: StringSQL -> ColumnSQL
columnSQL' =  ColumnSQL

-- | StringSQL from ColumnSQL
showsColumnSQL :: ColumnSQL -> StringSQL
showsColumnSQL (ColumnSQL c) = c

instance Show ColumnSQL where
  show = showStringSQL . showsColumnSQL

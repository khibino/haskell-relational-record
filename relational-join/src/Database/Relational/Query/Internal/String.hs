-- |
-- Module      : Database.Relational.Query.Internal.String
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides SQL string concatination functions
-- which result is ShowS differential lists.
module Database.Relational.Query.Internal.String (
  showUnwordsSQL, showWordSQL, showWordSQL',
  showUnwords, showSpace,

  paren, sqlRowString, sqlRowListString
  ) where

import Data.List (intercalate)

import qualified Language.SQL.Keyword as SQL

-- | Unwords 'SQL.Keyword' list and resturns 'ShowS'.
showUnwordsSQL :: [SQL.Keyword] -> ShowS
showUnwordsSQL =  showUnwords . map showWordSQL

-- | From 'SQL.Keyword' into 'ShowS'.
showWordSQL :: SQL.Keyword -> ShowS
showWordSQL =  showString . SQL.wordShow

-- | 'ShowS' of whitespace.
showSpace :: ShowS
showSpace =  showChar ' '

-- | From 'SQL.Keyword' with white into 'ShowS'.
showWordSQL' :: SQL.Keyword -> ShowS
showWordSQL' kw = showWordSQL kw . showSpace

-- | 'ShowS' version of unwords function.
showUnwords :: [ShowS] -> ShowS
showUnwords =  rec  where
  rec []     = id
  rec [s]    = s
  rec (s:ss@(_:_)) = s . showSpace . rec ss

-- | Parened String.
paren :: String -> String
paren =  ('(' :) . (++[')'])

-- | Row String of SQL values.
sqlRowString :: [String] -> String
sqlRowString = d where
  d ([])  = error $ "Projection: no columns."
  d ([c]) = c
  d (cs) =  paren $ intercalate ", " cs

-- | Rows String of SQL.
sqlRowListString :: [String] -> String
sqlRowListString =  paren . intercalate ", "

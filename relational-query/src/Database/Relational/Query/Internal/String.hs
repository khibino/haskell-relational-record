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
  showConcat, showSepBy, showUnwords, showSpace, showComma, showParen',
  showSqlRowString,

  paren, sqlRowString, sqlRowListString
  ) where

import Data.List (intersperse, intercalate)

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

-- | 'ShowS' of comma and space.
showComma :: ShowS
showComma =  showString ", "

-- | Paren 'ShowS'.
showParen' :: ShowS -> ShowS
showParen' =  showParen True

-- | From 'SQL.Keyword' with white into 'ShowS'.
showWordSQL' :: SQL.Keyword -> ShowS
showWordSQL' kw = showWordSQL kw . showSpace

-- | 'ShowS' version of concat function.
showConcat :: [ShowS] -> ShowS
showConcat =  foldr (.) id

-- | Separated 'ShowS' with delimitor.
showSepBy :: [ShowS] -> ShowS -> ShowS
showSepBy ts d = showConcat $ intersperse d ts

-- | 'ShowS' version of unwords function.
showUnwords :: [ShowS] -> ShowS
showUnwords =  (`showSepBy` showSpace)

-- | Parened String.
paren :: String -> String
paren =  ('(' :) . (++[')'])

showSqlRowString :: [ShowS] -> ShowS
showSqlRowString =  d  where
  d ([])  = error $ "Projection: no columns."
  d ([c]) = c
  d (cs) =  showParen' (cs `showSepBy` showComma)

-- | Row String of SQL values.
sqlRowString :: [String] -> String
sqlRowString = d where
  d ([])  = error $ "Projection: no columns."
  d ([c]) = c
  d (cs) =  paren $ intercalate ", " cs

-- | Rows String of SQL.
sqlRowListString :: [String] -> String
sqlRowListString =  paren . intercalate ", "

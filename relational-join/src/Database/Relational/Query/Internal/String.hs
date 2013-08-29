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
  showUnwordsSQL, showWordSQL, showUnwords,

  paren, sqlRowString
  ) where

import Data.List (intercalate)

import Language.SQL.Keyword (unwordsSQL)
import qualified Language.SQL.Keyword as SQL

-- | Unwords 'SQL.Keyword' list and resturns 'ShowS'.
showUnwordsSQL :: [SQL.Keyword] -> ShowS
showUnwordsSQL =  showString . unwordsSQL

-- | From 'SQL.Keyword' into 'ShowS'.
showWordSQL :: SQL.Keyword -> ShowS
showWordSQL =  showString . SQL.wordShow

-- | 'ShowS' version of unwords function.
showUnwords :: [ShowS] -> ShowS
showUnwords =  rec  where
  rec []     = id
  rec [s]    = s
  rec (s:ss@(_:_)) = s . showChar ' ' . rec ss

-- | Parened String.
paren :: String -> String
paren =  ('(' :) . (++[')'])

-- | Row String of SQL values.
sqlRowString :: [String] -> String
sqlRowString = d where
  d ([])  = error $ "Projection: no columns."
  d ([c]) = c
  d (cs) =  paren $ intercalate ", " cs

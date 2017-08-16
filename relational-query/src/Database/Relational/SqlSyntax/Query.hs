-- |
-- Module      : Database.Relational.SqlSyntax.Query
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides building and expanding operations of SQL query tree.
module Database.Relational.SqlSyntax.Query (
  showsDuplication,
  composeOrderBy,
  ) where

import Data.Monoid (Monoid (..), (<>))

import Language.SQL.Keyword (Keyword(..), (|*|), )
import qualified Language.SQL.Keyword as SQL

import Database.Relational.SqlSyntax.Types
  (Duplication (..), Order (..), Nulls (..), OrderingTerm, )
import Database.Relational.Internal.SQL (StringSQL)


-- | Compose duplication attribute string.
showsDuplication :: Duplication -> StringSQL
showsDuplication =  dup  where
  dup All      = ALL
  dup Distinct = DISTINCT


-- | Compose ORDER BY clause from OrderingTerms
composeOrderBy :: [OrderingTerm] -> StringSQL
composeOrderBy =  d where
  d []       = mempty
  d ts@(_:_) = ORDER <> BY <> SQL.fold (|*|) (map showsOt ts)
  showsOt ((o, mn), e) = e <> order o <> maybe mempty ((NULLS <>) . nulls) mn
  order Asc  = ASC
  order Desc = DESC
  nulls NullsFirst = FIRST
  nulls NullsLast  = LAST

-- |
-- Module      : Database.Relational.Query.Internal.BaseSQL
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides base structure of SQL syntax tree.
module Database.Relational.Query.Internal.BaseSQL (
  Duplication (..), showsDuplication,
  Order (..), OrderColumn, OrderingTerm, composeOrderBy,
  AssignColumn, AssignTerm, Assignment, composeSets, composeValues,
  ) where

import Data.Monoid (Monoid (..), (<>))

import Language.SQL.Keyword (Keyword(..), (|*|), (.=.))
import qualified Language.SQL.Keyword as SQL

import Database.Relational.Query.Internal.SQL
  (StringSQL, rowConsStringSQL)


-- | Result record duplication attribute
data Duplication = All | Distinct  deriving Show

-- | Compose duplication attribute string.
showsDuplication :: Duplication -> StringSQL
showsDuplication =  dup  where
  dup All      = ALL
  dup Distinct = DISTINCT


-- | Order direction. Ascendant or Descendant.
data Order = Asc | Desc  deriving Show

-- | Type for order-by column
type OrderColumn = StringSQL

-- | Type for order-by term
type OrderingTerm = (Order, OrderColumn)

-- | Compose ORDER BY clause from OrderingTerms
composeOrderBy :: [OrderingTerm] -> StringSQL
composeOrderBy =  d where
  d []       = mempty
  d ts@(_:_) = ORDER <> BY <> SQL.fold (|*|) (map showsOt ts)
  showsOt (o, e) = e <> order o
  order Asc  = ASC
  order Desc = DESC


-- | Column SQL String of assignment
type AssignColumn = StringSQL

-- | Value SQL String of assignment
type AssignTerm   = StringSQL

-- | Assignment pair
type Assignment = (AssignColumn, AssignTerm)

-- | Compose SET clause from ['Assignment'].
composeSets :: [Assignment] -> StringSQL
composeSets as = assigns  where
  assignList = foldr (\ (col, term) r ->
                       (col .=. term) : r)
               [] as
  assigns | null assignList = error "Update assignment list is null!"
          | otherwise       = SET <> SQL.fold (|*|) assignList

-- | Compose VALUES clause from ['Assignment'].
composeValues :: [Assignment] -> StringSQL
composeValues as = rowConsStringSQL cs <> VALUES <> rowConsStringSQL vs  where
  (cs, vs) = unzip as

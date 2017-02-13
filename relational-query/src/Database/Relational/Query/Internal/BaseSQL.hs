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
  Duplication (..),
  Order (..), OrderColumn, OrderingTerm,
  AssignColumn, AssignTerm, Assignment,
  ) where

import Database.Relational.Query.Internal.SQL (ColumnSQL)


-- | Result record duplication attribute
data Duplication = All | Distinct  deriving Show

-- | Order direction. Ascendant or Descendant.
data Order = Asc | Desc  deriving Show

-- | Type for order-by column
type OrderColumn = ColumnSQL

-- | Type for order-by term
type OrderingTerm = (Order, OrderColumn)

-- | Column SQL String of assignment
type AssignColumn = ColumnSQL

-- | Value SQL String of assignment
type AssignTerm   = ColumnSQL

-- | Assignment pair
type Assignment = (AssignColumn, AssignTerm)

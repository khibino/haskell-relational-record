-- |
-- Module      : Database.Relational.Query.Internal.UntypedTable
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines no-phantom table type which has table metadatas.
module Database.Relational.Query.Internal.UntypedTable (
  Untyped (Untyped), name', width', columns', (!),
  ) where

import Data.Array (Array, elems)
import qualified Data.Array as Array

import Database.Relational.Query.Internal.SQL (ColumnSQL)


-- | Untyped typed table type
data Untyped = Untyped String Int (Array Int ColumnSQL)  deriving Show

-- | Name string of table in SQL
name' :: Untyped -> String
name'       (Untyped n _ _) = n

-- | Width of table
width' :: Untyped -> Int
width'      (Untyped _ w _) = w

-- | Column name strings in SQL
columnArray :: Untyped -> Array Int ColumnSQL
columnArray (Untyped _ _ c) = c

-- | Column name strings in SQL
columns' :: Untyped -> [ColumnSQL]
columns' =  elems . columnArray

-- | Column name string in SQL specified by index
(!) :: Untyped
    -> Int       -- ^ Column index
    -> ColumnSQL -- ^ Column name String in SQL
t ! i = columnArray t Array.! i

-- |
-- Module      : Database.Relational.Internal.UntypedTable
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines no-phantom table type which has table metadatas.
module Database.Relational.Internal.UntypedTable (
  Untyped (Untyped), name', width', columns', (!),
  ) where

import Data.Array (Array, elems)
import qualified Data.Array as Array

import Database.Relational.Internal.String (StringSQL)


-- | Untyped typed table type
data Untyped = Untyped String Int (Array Int StringSQL)  deriving Show

-- | Name string of table in SQL
name' :: Untyped -> String
name'       (Untyped n _ _) = n

-- | Width of table
width' :: Untyped -> Int
width'      (Untyped _ w _) = w

-- | Column name strings in SQL
columnArray :: Untyped -> Array Int StringSQL
columnArray (Untyped _ _ c) = c

-- | Column name strings in SQL
columns' :: Untyped -> [StringSQL]
columns' =  elems . columnArray

-- | Column name string in SQL specified by index
(!) :: Untyped
    -> Int       -- ^ Column index
    -> StringSQL -- ^ Column name String in SQL
t ! i = columnArray t Array.! i

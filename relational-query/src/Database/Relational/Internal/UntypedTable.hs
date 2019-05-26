-- |
-- Module      : Database.Relational.Internal.UntypedTable
-- Copyright   : 2013-2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines no-phantom table type which has table metadatas.
module Database.Relational.Internal.UntypedTable (
  Untyped,
  UTable (UTable), name', width', columns', (!),
  ) where

import Data.Array (Array, elems)
import qualified Data.Array as Array

import Database.Relational.Internal.String (StringSQL)


type Untyped = UTable
{-# DEPRECATED Untyped "use UTable" #-}

-- | UTable - un-record-typed table type
data UTable = UTable String Int (Array Int StringSQL)  deriving Show

-- | Name string of table in SQL
name' :: UTable -> String
name'       (UTable n _ _) = n

-- | Width of table
width' :: UTable -> Int
width'      (UTable _ w _) = w

-- | Column name strings in SQL
columnArray :: UTable -> Array Int StringSQL
columnArray (UTable _ _ c) = c

-- | Column name strings in SQL
columns' :: UTable -> [StringSQL]
columns' =  elems . columnArray

-- | Column name string in SQL specified by index
(!) :: UTable
    -> Int       -- ^ Column index
    -> StringSQL -- ^ Column name String in SQL
t ! i = columnArray t Array.! i

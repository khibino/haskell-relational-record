-- |
-- Module      : Database.Relational.Query.Table
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines table type which has table metadatas.
module Database.Relational.Query.Table (
  -- * Type for column SQL string
  ColumnSQL, columnSQL, sqlWordFromColumn, stringFromColumnSQL,

  -- * Untyped table type
  Untyped, name', width', columns', (!),

  -- * Phantom typed table type
  Table, unType, name, shortName, width, columns, index, table, toMaybe,
  ) where

import Data.Array (Array, listArray, elems)
import qualified Data.Array as Array

import qualified Language.SQL.Keyword as SQL


-- | Column SQL string type
newtype ColumnSQL = ColumnSQL String

-- | 'ColumnSQL' from string
columnSQL :: String -> ColumnSQL
columnSQL =  ColumnSQL

-- | String from ColumnSQL
stringFromColumnSQL :: ColumnSQL -> String
stringFromColumnSQL (ColumnSQL s) = s

-- | SQL word from 'ColumnSQL'
sqlWordFromColumn :: ColumnSQL -> SQL.Keyword
sqlWordFromColumn =  SQL.word . stringFromColumnSQL


-- | Untyped typed table type
data Untyped = Untyped String Int (Array Int ColumnSQL)

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


-- | Phantom typed table type
newtype Table r = Table Untyped

-- | Untype table.
unType :: Table t -> Untyped
unType (Table u) = u

-- | Name string of table in SQL
name :: Table r -> String
name   = name'   . unType

-- | Not qualified name string of table in SQL
shortName :: Table r -> String
shortName =  tail . dropWhile (/= '.') . name

-- | Width of table
width :: Table r -> Int
width  = width'  . unType

-- | Column name strings in SQL
columns :: Table r -> [ColumnSQL]
columns =  columns' . unType

-- | Column name string in SQL specified by index
index :: Table r
      -> Int    -- ^ Column index
      -> ColumnSQL -- ^ Column name String in SQL
index =  (!) . unType

-- | Cast phantom type into 'Maybe' type.
toMaybe :: Table r -> Table (Maybe r)
toMaybe (Table t) = (Table t)

-- | Unsafely generate phantom typed table type.
table :: String -> [String] -> Table r
table n f = Table $ Untyped n w fa  where
  w  = length f
  fa = listArray (0, w - 1) $ map columnSQL f

-- |
-- Module      : Database.Relational.Query.Table
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines table type which has table metadatas.
module Database.Relational.Query.Table (
  -- * Untyped table type
  -- deprecated interfaces
  Untyped, name', width', columns', (!),

  -- * Phantom typed table type
  Table, unType, name, shortName, width, columns, index, table, toMaybe,

  -- * Table existence inference
  TableDerivable (..)
  ) where

import Data.Array (listArray)

import Database.Record (PersistableWidth)

import qualified Database.Relational.Query.Internal.UntypedTable as Untyped
import Database.Relational.Query.Internal.SQL (ColumnSQL, columnSQL)


{-# DEPRECATED Untyped, name', width', columns', (!) "prepare to drop public interface. internally use Database.Relational.Query.Internal.UntypedTable.*" #-}
-- | Untyped typed table type
type Untyped = Untyped.Untyped

-- | Name string of table in SQL
name' :: Untyped -> String
name' = Untyped.name'

-- | Width of table
width' :: Untyped -> Int
width' = Untyped.width'

-- | Column name strings in SQL
columns' :: Untyped -> [ColumnSQL]
columns' = Untyped.columns'

-- | Column name strings in SQL
(!) :: Untyped -> Int -> ColumnSQL
(!) = (Untyped.!)

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
      -> Int       -- ^ Column index
      -> ColumnSQL -- ^ Column name String in SQL
index =  (!) . unType

-- | Cast phantom type into 'Maybe' type.
toMaybe :: Table r -> Table (Maybe r)
toMaybe (Table t) = Table t

-- | Unsafely generate phantom typed table type.
table :: String -> [String] -> Table r
table n f = Table $ Untyped.Untyped n w fa  where
  w  = length f
  fa = listArray (0, w - 1) $ map columnSQL f

-- | Inference rule of 'Table' existence.
class PersistableWidth r => TableDerivable r where
  derivedTable :: Table r

-- |
-- Module      : Database.Relational.Typed.Table
-- Copyright   : 2013-2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines table type which has table metadatas.
module Database.Relational.Typed.Table (
  -- * Phantom typed table type
  Table, untype, name, shortName, width, columns, index, table, recordWidth,

  toSubQuery,

  -- * Table existence inference
  TableDerivable (..)
  ) where

import Data.Array (listArray)

import Database.Record.Persistable
  (PersistableWidth, PersistableRecordWidth, unsafePersistableRecordWidth)

import Database.Relational.Internal.UntypedTable (UTable (UTable), name', width', columns', (!))
import Database.Relational.Internal.String (StringSQL, stringSQL, showStringSQL)
import Database.Relational.SqlSyntax (SubQuery)
import qualified Database.Relational.SqlSyntax as Syntax


-- | Phantom typed table type
newtype Table r = Table UTable

instance Show (Table r) where
  show t =
    unwords
    ["Table",  show $ name t,
     show . map showStringSQL $ columns t]

-- | Untype table.
untype :: Table t -> UTable
untype (Table u) = u

-- | Name string of table in SQL
name :: Table r -> String
name = name' . untype

-- | Not qualified name string of table in SQL
shortName :: Table r -> String
shortName = tail . dropWhile (/= '.') . name

-- | Width of table
width :: Table r -> Int
width = width' . untype

-- | Column name strings in SQL
columns :: Table r -> [StringSQL]
columns = columns' . untype

-- | Column name string in SQL specified by index
index :: Table r
      -> Int       -- ^ Column index
      -> StringSQL -- ^ Column name String in SQL
index = (!) . untype

-- | Unsafely generate phantom typed table type.
table :: String -> [String] -> Table r
table n f = Table $ UTable n w fa  where
  w  = length f
  fa = listArray (0, w - 1) $ map stringSQL f

-- | 'SubQuery' from 'Table'.
toSubQuery :: Table r  -- ^ Typed 'Table' metadata
           -> SubQuery -- ^ Result 'SubQuery'
toSubQuery = Syntax.Table . untype

-- | Inference rule of 'Table' existence.
class PersistableWidth r => TableDerivable r where
  derivedTable :: Table r

-- | PersistableRecordWidth of table
recordWidth :: Table r -> PersistableRecordWidth r
recordWidth = unsafePersistableRecordWidth . width

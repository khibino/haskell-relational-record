-- |
-- Module      : Database.Relational.Query.Component
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides untyped components for query.
module Database.Relational.Query.Component (
  -- * Type for column SQL string
  ColumnSQL, columnSQL, sqlWordFromColumn, stringFromColumnSQL
  ) where

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

-- |
-- Module      : Database.Relational.Table
-- Copyright   : 2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines interfaces about table metadata type.
module Database.Relational.Table (
  -- * Phantom typed table type
  Table, untype, name, shortName, width, columns, index, table,

  -- * Table existence inference
  TableDerivable (..)
  ) where

import Database.Relational.Typed.Table

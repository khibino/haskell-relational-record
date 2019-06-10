-- |
-- Module      : Database.Relational.HDBC
-- Copyright   : 2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides merged namespace of
-- typed 'Query', 'Insert', 'InsertQuery', 'Update', 'KeyUpdate' and 'Delete'
-- running sequences.
module Database.Relational.HDBC (
  module Database.Relational.HDBC.Query,
  module Database.Relational.HDBC.Insert,
  module Database.Relational.HDBC.InsertQuery,
  module Database.Relational.HDBC.Update,
  module Database.Relational.HDBC.KeyUpdate,
  module Database.Relational.HDBC.Delete,
  module Database.Relational.HDBC.Statement
  ) where

import Database.Relational.HDBC.Query hiding (prepare)
import Database.Relational.HDBC.Insert hiding (prepare)
import Database.Relational.HDBC.InsertQuery hiding (prepare)
import Database.Relational.HDBC.Update hiding (prepare)
import Database.Relational.HDBC.KeyUpdate hiding (prepare)
import Database.Relational.HDBC.Delete hiding (prepare)
import Database.Relational.HDBC.Statement

{-# ANN module "HLint: ignore Use import/export shortcut" #-}

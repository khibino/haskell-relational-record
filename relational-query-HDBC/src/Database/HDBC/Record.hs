-- |
-- Module      : Database.HDBC.Record
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides merged namespace of
-- typed 'Query', 'Insert', 'InsertQuery', 'Update', 'KeyUpdate' and 'Delete'
-- running sequences.
module Database.HDBC.Record (
  module Database.HDBC.Record.Query,
  module Database.HDBC.Record.Insert,
  module Database.HDBC.Record.InsertQuery,
  module Database.HDBC.Record.Update,
  module Database.HDBC.Record.KeyUpdate,
  module Database.HDBC.Record.Delete,
  module Database.HDBC.Record.Statement
                            ) where

import Database.HDBC.Record.Query hiding (prepare)
import Database.HDBC.Record.Insert hiding (prepare)
import Database.HDBC.Record.InsertQuery hiding (prepare)
import Database.HDBC.Record.Update hiding (prepare)
import Database.HDBC.Record.KeyUpdate hiding (prepare)
import Database.HDBC.Record.Delete hiding (prepare)
import Database.HDBC.Record.Statement

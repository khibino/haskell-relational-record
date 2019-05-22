-- |
-- Module      : Database.Custom.MySQL
-- Copyright   : 2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides custom APIs with appropriate configuration
-- for MySQL.
module Database.Custom.MySQL (
  module Database.Relational,

  relationalQuery,

  insertValue, insertValueNoPH, insertQuery,
  update, updateNoPH,
  delete, deleteNoPH,
  ) where

import Language.SQL.Keyword (Keyword)
import Database.Record (PersistableWidth)
import Database.Relational.Schema.MySQL.Config (config)
import Database.Relational hiding
  (relationalQuery,
   insertValue, insertValueNoPH, insertQuery,
   update, updateNoPH,
   delete, deleteNoPH, )

-- | From 'Relation' into typed 'Query' with suffix SQL words.
relationalQuery :: PersistableWidth p
                => Relation p r  -- ^ relation to finalize building
                -> [Keyword]     -- ^ suffix SQL words. for example, `[FOR, UPDATE]`, `[FETCH, FIRST, "3", ROWS, ONLY]` ...
                -> Query p r     -- ^ finalized query
relationalQuery = relationalQuery_ config

-- | Make 'Insert' from derived table and monadic builded 'Register' object.
insertValue :: (PersistableWidth r, TableDerivable r, PersistableWidth p)
            => ReadPlaceholders p (Register r) ()
            -> Insert p
insertValue = insertValue' config

-- | Make 'Insert' from derived table and monadic builded 'Register' object with no(unit) placeholder.
insertValueNoPH :: TableDerivable r
                => ReadPlaceholders () (Register r) ()
                -> Insert ()
insertValueNoPH = insertValue

-- | Make 'InsertQuery' from derived table, 'Pi' and 'Relation'.
insertQuery :: (PersistableWidth p, TableDerivable r) => Pi r r' -> Relation p r' -> InsertQuery p
insertQuery = insertQuery' config

-- | Make 'Update' from derived table and 'Assign' computation.
update :: (PersistableWidth p, TableDerivable r)
       => (Record Flat r -> ReadPlaceholders p (Assign r) ())
       -> Update p
update = update' config

-- | Make 'Update' from derived table and 'Assign' computation with no(unit) placeholder.
updateNoPH :: TableDerivable r
           => (Record Flat r -> ReadPlaceholders () (Assign r) ())
           -> Update ()
updateNoPH = update

-- | Make 'Delete' from derived table and 'Restrict' computation.
delete :: (PersistableWidth p, TableDerivable r)
       => (Record Flat r -> ReadPlaceholders p Restrict ())
       -> Delete p
delete = delete' config

-- | Make 'Delete' from 'defaultConfig', derived table and 'Restrict' computation with no(unit) placeholder.
deleteNoPH :: TableDerivable r
           => (Record Flat r -> ReadPlaceholders () Restrict ())
           -> Delete ()
deleteNoPH = delete

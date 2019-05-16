-- |
-- Module      : Database.Custom.SQLServer
-- Copyright   : 2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides custom APIs with appropriate configuration
-- for SQLServer.
module Database.Custom.SQLServer (
  module Database.Relational,

  relationalQuery,

  insertValue, insertValueNoPH, insertQuery,
  update, updateNoPH,
  delete, deleteNoPH,
  ) where

import Language.SQL.Keyword (Keyword)
import Database.Relational.Schema.SQLServer.Config (config)
import Database.Relational hiding
  (relationalQuery,
   insertValue, insertValueNoPH, insertQuery,
   update, updateNoPH,
   delete, deleteNoPH, )

-- | From 'Relation' into typed 'Query' with suffix SQL words.
relationalQuery :: Relation p r  -- ^ relation to finalize building
                -> [Keyword]     -- ^ suffix SQL words. for example, `[FOR, UPDATE]`, `[FETCH, FIRST, "3", ROWS, ONLY]` ...
                -> Query p r     -- ^ finalized query
relationalQuery = relationalQuery_ config

-- | Make 'Insert' from derived table and monadic builded 'Register' object.
insertValue :: TableDerivable r
            => Register r (PlaceHolders p)
            -> Insert p
insertValue = insertValue' config

-- | Make 'Insert' from derived table and monadic builded 'Register' object with no(unit) placeholder.
insertValueNoPH :: TableDerivable r
                => Register r ()
                -> Insert ()
insertValueNoPH body = insertValue $ body >> return unitPH

-- | Make 'InsertQuery' from derived table, 'Pi' and 'Relation'.
insertQuery :: TableDerivable r => Pi r r' -> Relation p r' -> InsertQuery p
insertQuery = insertQuery' config

-- | Make 'Update' from derived table and 'Assign' computation.
update :: TableDerivable r
       => (Record Flat r -> Assign r (PlaceHolders p))
       -> Update p
update = update' config

-- | Make 'Update' from derived table and 'Assign' computation with no(unit) placeholder.
updateNoPH :: TableDerivable r
           => (Record Flat r -> Assign r ())
           -> Update ()
updateNoPH body = update $ (>> return unitPH) . body

-- | Make 'Delete' from derived table and 'Restrict' computation.
delete :: TableDerivable r
       => (Record Flat r -> Restrict (PlaceHolders p))
       -> Delete p
delete = delete' config

-- | Make 'Delete' from 'defaultConfig', derived table and 'Restrict' computation with no(unit) placeholder.
deleteNoPH :: TableDerivable r
           => (Record Flat r -> Restrict ())
           -> Delete ()
deleteNoPH body = delete $ (>> return unitPH) . body

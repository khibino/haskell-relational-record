-- |
-- Module      : Database.Relational.Query.Type
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines typed SQL.
module Database.Relational.Query.Type (
  -- * Typed query statement
  Query (..), unsafeTypedQuery,

  relationalQuery', relationalQuery,

  relationalQuerySQL,

  -- * Typed update statement
  KeyUpdate (..), unsafeTypedKeyUpdate, typedKeyUpdate, typedKeyUpdateTable,
  Update (..), unsafeTypedUpdate, typedUpdate, typedUpdateTable, targetUpdate, targetUpdateTable,
  typedUpdateAllColumn, restrictedUpdateAllColumn, restrictedUpdateTableAllColumn,

  updateSQL,

  -- * Typed insert statement
  Insert (..), unsafeTypedInsert, typedInsert, derivedInsert,
  InsertQuery (..), unsafeTypedInsertQuery, typedInsertQuery, derivedInsertQuery,

  insertQuerySQL,

  -- * Typed delete statement
  Delete (..), unsafeTypedDelete, typedDelete, restrictedDelete,

  deleteSQL,

  -- * Generalized interfaces
  UntypeableNoFetch (..)
  ) where

import Data.Monoid ((<>))

import Database.Record (PersistableWidth)

import Database.Relational.Query.Internal.SQL (showStringSQL)
import Database.Relational.Query.Relation (Relation, sqlFromRelationWith, tableOf)
import Database.Relational.Query.Restriction
  (Restriction, RestrictionContext, restriction',
   UpdateTarget, UpdateTargetContext, updateTarget', liftTargetAllColumn',
   sqlWhereFromRestriction, sqlFromUpdateTarget)
import Database.Relational.Query.Pi (Pi)
import qualified Database.Relational.Query.Pi as Pi
import Database.Relational.Query.Component (Config, defaultConfig)
import Database.Relational.Query.Table (Table, TableDerivable, derivedTable)
import Database.Relational.Query.SQL
  (QuerySuffix, showsQuerySuffix,
   updateOtherThanKeySQL, insertPrefixSQL, insertSQL, updatePrefixSQL, deletePrefixSQL)


-- | Query type with place-holder parameter 'p' and query result type 'a'.
newtype Query p a = Query { untypeQuery :: String }

-- | Unsafely make typed 'Query' from SQL string.
unsafeTypedQuery :: String    -- ^ Query SQL to type
                 -> Query p a -- ^ Typed result
unsafeTypedQuery =  Query

-- | Show query SQL string
instance Show (Query p a) where
  show = untypeQuery

-- | From 'Relation' into untyped SQL query string.
relationalQuerySQL :: Config -> Relation p r -> QuerySuffix -> String
relationalQuerySQL config rel qsuf = showStringSQL $ sqlFromRelationWith rel config <> showsQuerySuffix qsuf

-- | From 'Relation' into typed 'Query' with suffix SQL words.
relationalQuery' :: Relation p r -> QuerySuffix -> Query p r
relationalQuery' rel qsuf = unsafeTypedQuery $ relationalQuerySQL defaultConfig rel qsuf

-- | From 'Relation' into typed 'Query'.
relationalQuery :: Relation p r -> Query p r
relationalQuery =  (`relationalQuery'` [])


-- | Update type with key type 'p' and update record type 'a'.
--   Columns to update are record columns other than key columns,
--   So place-holder parameter type is the same as record type 'a'.
data KeyUpdate p a = KeyUpdate { updateKey :: Pi a p
                               , untypeKeyUpdate :: String
                               }

-- | Unsafely make typed 'KeyUpdate' from SQL string.
unsafeTypedKeyUpdate :: Pi a p -> String -> KeyUpdate p a
unsafeTypedKeyUpdate =  KeyUpdate

-- | Make typed 'KeyUpdate' from 'Table' and key indexes.
typedKeyUpdate :: Table a -> Pi a p -> KeyUpdate p a
typedKeyUpdate tbl key = unsafeTypedKeyUpdate key $ updateOtherThanKeySQL tbl key

-- | Make typed 'KeyUpdate' object using derived info specified by 'Relation' type.
typedKeyUpdateTable :: TableDerivable r => Relation () r -> Pi r p -> KeyUpdate p r
typedKeyUpdateTable =  typedKeyUpdate . tableOf

-- | Show update SQL string
instance Show (KeyUpdate p a) where
  show = untypeKeyUpdate


-- | Update type with place-holder parameter 'p'.
newtype Update p = Update { untypeUpdate :: String }

-- | Unsafely make typed 'Update' from SQL string.
unsafeTypedUpdate :: String -> Update p
unsafeTypedUpdate =  Update

-- | Make untyped update SQL string from 'Table' and 'Restriction'.
updateSQL :: Table r -> UpdateTarget p r -> String
updateSQL tbl ut = showStringSQL $ updatePrefixSQL tbl <> sqlFromUpdateTarget tbl ut

-- | Make typed 'Update' from 'Table' and 'Restriction'.
typedUpdate :: Table r -> UpdateTarget p r -> Update p
typedUpdate tbl ut = unsafeTypedUpdate $ updateSQL tbl ut

-- | Make typed 'Update' object using derived info specified by 'Relation' type.
typedUpdateTable :: TableDerivable r => Relation () r -> UpdateTarget p r -> Update p
typedUpdateTable =  typedUpdate . tableOf

-- | Directly make typed 'Update' from 'Table' and 'Target' monad context.
targetUpdate :: Table r
             -> UpdateTargetContext p r -- ^ 'Target' monad context
             -> Update p
targetUpdate tbl = typedUpdate tbl . updateTarget'

-- | Directly make typed 'Update' from 'Relation' and 'Target' monad context.
targetUpdateTable :: TableDerivable r
                  => Relation () r
                  -> UpdateTargetContext p r -- ^ 'Target' monad context
                  -> Update p
targetUpdateTable =  targetUpdate . tableOf

-- | Make typed 'Update' from 'Table' and 'Restriction'.
--   Update target is all column.
typedUpdateAllColumn :: PersistableWidth r
                     => Table r
                     -> Restriction p r
                     -> Update (r, p)
typedUpdateAllColumn tbl r = typedUpdate tbl $ liftTargetAllColumn' r

-- | Directly make typed 'Update' from 'Table' and 'Restrict' monad context.
--   Update target is all column.
restrictedUpdateAllColumn :: PersistableWidth r
                           => Table r
                           -> RestrictionContext p r -- ^ 'Restrict' monad context
                           -> Update (r, p)
restrictedUpdateAllColumn tbl = typedUpdateAllColumn tbl . restriction'

-- | Directly make typed 'Update' from 'Table' and 'Restrict' monad context.
--   Update target is all column.
restrictedUpdateTableAllColumn :: (PersistableWidth r, TableDerivable r)
                               => Relation () r
                               -> RestrictionContext p r
                               -> Update (r, p)
restrictedUpdateTableAllColumn =  restrictedUpdateAllColumn . tableOf

-- | Show update SQL string
instance Show (Update p) where
  show = untypeUpdate


-- | Insert type to insert record type 'a'.
newtype Insert a   = Insert { untypeInsert :: String }

-- | Unsafely make typed 'Insert' from SQL string.
unsafeTypedInsert :: String -> Insert a
unsafeTypedInsert =  Insert

-- | Make typed 'Insert' from columns selector 'Pi' and 'Table'.
typedInsert :: Pi r r' -> Table r -> Insert r'
typedInsert pi' =  unsafeTypedInsert . insertSQL pi'

-- | Infered 'Insert'.
derivedInsert :: TableDerivable r => Insert r
derivedInsert =  typedInsert Pi.id' derivedTable

-- | Show insert SQL string.
instance Show (Insert a) where
  show = untypeInsert

-- | InsertQuery type.
newtype InsertQuery p = InsertQuery { untypeInsertQuery :: String }

-- | Unsafely make typed 'InsertQuery' from SQL string.
unsafeTypedInsertQuery :: String -> InsertQuery p
unsafeTypedInsertQuery =  InsertQuery

-- | Make untyped insert select SQL string from 'Table' and 'Relation'.
insertQuerySQL :: Config -> Pi r r' -> Table r -> Relation p r' -> String
insertQuerySQL config pi' tbl rel = showStringSQL $ insertPrefixSQL pi' tbl <> sqlFromRelationWith rel config

-- | Make typed 'InsertQuery' from columns selector 'Pi' and 'Table' and 'Relation'.
typedInsertQuery :: Pi r r' -> Table r -> Relation p r' -> InsertQuery p
typedInsertQuery pi' tbl rel = unsafeTypedInsertQuery $ insertQuerySQL defaultConfig pi' tbl rel

-- | Infered 'InsertQuery'.
derivedInsertQuery :: TableDerivable r => Relation p r -> InsertQuery p
derivedInsertQuery =  typedInsertQuery Pi.id' derivedTable

-- | Show insert SQL string.
instance Show (InsertQuery p) where
  show = untypeInsertQuery


-- | Delete type with place-holder parameter 'p'.
newtype Delete p = Delete { untypeDelete :: String }

-- | Unsafely make typed 'Delete' from SQL string.
unsafeTypedDelete :: String -> Delete p
unsafeTypedDelete =  Delete

-- | Make untyped delete SQL string from 'Table' and 'Restriction'.
deleteSQL :: Table r -> Restriction p r -> String
deleteSQL tbl r = showStringSQL $ deletePrefixSQL tbl <> sqlWhereFromRestriction tbl r

-- | Make typed 'Delete' from 'Table' and 'Restriction'.
typedDelete :: Table r -> Restriction p r -> Delete p
typedDelete tbl r = unsafeTypedDelete $ deleteSQL tbl r

-- | Directly make typed 'Delete' from 'Table' and 'Restrict' monad context.
restrictedDelete :: Table r
                 -> RestrictionContext p r -- ^ 'Restrict' monad context.
                 -> Delete p
restrictedDelete tbl = typedDelete tbl . restriction'

-- | Show delete SQL string
instance Show (Delete p) where
  show = untypeDelete


-- | Untype interface for typed no-result type statments
--   with single type parameter which represents place-holder parameter 'p'.
class UntypeableNoFetch s where
  untypeNoFetch :: s p -> String

instance UntypeableNoFetch Insert where
  untypeNoFetch = untypeInsert

instance UntypeableNoFetch Update where
  untypeNoFetch = untypeUpdate

instance UntypeableNoFetch Delete where
  untypeNoFetch = untypeDelete

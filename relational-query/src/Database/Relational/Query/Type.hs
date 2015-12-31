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
  Update (..), unsafeTypedUpdate, typedUpdate', typedUpdate, derivedUpdate', derivedUpdate,
  typedUpdateAllColumn, restrictedUpdateAllColumn, restrictedUpdateTableAllColumn,

  updateSQL,

  -- * Typed insert statement
  Insert (..), unsafeTypedInsert', unsafeTypedInsert, typedInsert', typedInsert, derivedInsert,
  InsertQuery (..), unsafeTypedInsertQuery, typedInsertQuery', typedInsertQuery, derivedInsertQuery,

  insertQuerySQL,

  -- * Typed delete statement
  Delete (..), unsafeTypedDelete, typedDelete', typedDelete, derivedDelete', derivedDelete,

  deleteSQL,

  -- * Generalized interfaces
  UntypeableNoFetch (..)
  ) where

import Data.Monoid ((<>))

import Database.Record (PersistableWidth)

import Database.Relational.Query.Internal.SQL (showStringSQL)
import Database.Relational.Query.Monad.BaseType (Relation, sqlFromRelationWith)
import Database.Relational.Query.Monad.Restrict (RestrictedStatement)
import Database.Relational.Query.Monad.Assign (AssignStatement)
import Database.Relational.Query.Relation (tableOf)
import Database.Relational.Query.Effect
  (Restriction, restriction', UpdateTarget, updateTarget', liftTargetAllColumn',
   sqlWhereFromRestriction, sqlFromUpdateTarget)
import Database.Relational.Query.Pi (Pi)
import Database.Relational.Query.Component (Config (chunksInsertSize), defaultConfig)
import Database.Relational.Query.Table (Table, TableDerivable, derivedTable)
import Database.Relational.Query.Projectable (PlaceHolders)
import Database.Relational.Query.SQL
  (QuerySuffix, showsQuerySuffix, insertPrefixSQL, insertSQL, insertSizedChunkSQL,
   updateOtherThanKeySQL, updatePrefixSQL, deletePrefixSQL)


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

-- | Make typed 'KeyUpdate' from 'Table' and key columns selector 'Pi'.
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

-- | Make untyped update SQL string from 'Table' and 'UpdateTarget'.
updateSQL :: Config -> Table r -> UpdateTarget p r -> String
updateSQL config tbl ut = showStringSQL $ updatePrefixSQL tbl <> sqlFromUpdateTarget config tbl ut

-- | Make typed 'Update' from 'Config', 'Table' and 'UpdateTarget'.
typedUpdate' :: Config -> Table r -> UpdateTarget p r -> Update p
typedUpdate' config tbl ut = unsafeTypedUpdate $ updateSQL config tbl ut

-- | Make typed 'Update' using 'defaultConfig', 'Table' and 'UpdateTarget'.
typedUpdate :: Table r -> UpdateTarget p r -> Update p
typedUpdate =  typedUpdate' defaultConfig

targetTable :: TableDerivable r => UpdateTarget p r -> Table r
targetTable =  const derivedTable

-- | Make typed 'Update' from 'Config', derived table and 'AssignStatement'
derivedUpdate' :: TableDerivable r => Config -> AssignStatement r (PlaceHolders p) -> Update p
derivedUpdate' config utc =  typedUpdate' config (targetTable ut) ut  where
  ut = updateTarget' utc

-- | Make typed 'Update' from 'defaultConfig', derived table and 'AssignStatement'
derivedUpdate :: TableDerivable r => AssignStatement r (PlaceHolders p) -> Update p
derivedUpdate = derivedUpdate' defaultConfig


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
                           -> RestrictedStatement r (PlaceHolders p) -- ^ 'Restrict' monad context
                           -> Update (r, p)
restrictedUpdateAllColumn tbl = typedUpdateAllColumn tbl . restriction'

-- | Directly make typed 'Update' from 'Table' and 'Restrict' monad context.
--   Update target is all column.
restrictedUpdateTableAllColumn :: (PersistableWidth r, TableDerivable r)
                               => Relation () r
                               -> RestrictedStatement r (PlaceHolders p)
                               -> Update (r, p)
restrictedUpdateTableAllColumn =  restrictedUpdateAllColumn . tableOf

-- | Show update SQL string
instance Show (Update p) where
  show = untypeUpdate


-- | Insert type to insert record type 'a'.
data Insert a   =
  Insert
  { untypeInsert :: String
  , untypeChunkInsert :: String
  , chunkSizeOfInsert :: Int
  }

-- | Unsafely make typed 'Insert' from single insert and chunked insert SQL.
unsafeTypedInsert' :: String -> String -> Int -> Insert a
unsafeTypedInsert' =  Insert

-- | Unsafely make typed 'Insert' from single insert SQL.
unsafeTypedInsert :: String -> Insert a
unsafeTypedInsert q = unsafeTypedInsert' q q 1

-- | Make typed 'Insert' from 'Table' and columns selector 'Pi' with configuration parameter.
typedInsert' :: Config -> Table r -> Pi r r' -> Insert r'
typedInsert' config tbl pi' = unsafeTypedInsert' (insertSQL pi' tbl) ci n  where
  (ci, n) = insertSizedChunkSQL pi' tbl $ chunksInsertSize config

-- | Make typed 'Insert' from 'Table' and columns selector 'Pi'.
typedInsert :: Table r -> Pi r r' -> Insert r'
typedInsert =  typedInsert' defaultConfig

-- | Table type inferred 'Insert'.
derivedInsert :: TableDerivable r => Pi r r' -> Insert r'
derivedInsert =  typedInsert derivedTable

-- | Show insert SQL string.
instance Show (Insert a) where
  show = untypeInsert

-- | InsertQuery type.
newtype InsertQuery p = InsertQuery { untypeInsertQuery :: String }

-- | Unsafely make typed 'InsertQuery' from SQL string.
unsafeTypedInsertQuery :: String -> InsertQuery p
unsafeTypedInsertQuery =  InsertQuery

-- | Make untyped insert select SQL string from 'Table', 'Pi' and 'Relation'.
insertQuerySQL :: Config -> Table r -> Pi r r' -> Relation p r' -> String
insertQuerySQL config tbl pi' rel = showStringSQL $ insertPrefixSQL pi' tbl <> sqlFromRelationWith rel config

-- | Make typed 'InsertQuery' from columns selector 'Table', 'Pi' and 'Relation' with configuration parameter.
typedInsertQuery' :: Config -> Table r -> Pi r r' -> Relation p r' -> InsertQuery p
typedInsertQuery' config tbl pi' rel = unsafeTypedInsertQuery $ insertQuerySQL config tbl pi' rel

-- | Make typed 'InsertQuery' from columns selector 'Table', 'Pi' and 'Relation'.
typedInsertQuery :: Table r -> Pi r r' -> Relation p r' -> InsertQuery p
typedInsertQuery =  typedInsertQuery' defaultConfig

-- | Table type inferred 'InsertQuery'.
derivedInsertQuery :: TableDerivable r => Pi r r' -> Relation p r' -> InsertQuery p
derivedInsertQuery =  typedInsertQuery derivedTable

-- | Show insert SQL string.
instance Show (InsertQuery p) where
  show = untypeInsertQuery


-- | Delete type with place-holder parameter 'p'.
newtype Delete p = Delete { untypeDelete :: String }

-- | Unsafely make typed 'Delete' from SQL string.
unsafeTypedDelete :: String -> Delete p
unsafeTypedDelete =  Delete

-- | Make untyped delete SQL string from 'Table' and 'Restriction'.
deleteSQL :: Config -> Table r -> Restriction p r -> String
deleteSQL config tbl r = showStringSQL $ deletePrefixSQL tbl <> sqlWhereFromRestriction config tbl r

-- | Make typed 'Delete' from 'Config', 'Table' and 'Restriction'.
typedDelete' :: Config -> Table r -> Restriction p r -> Delete p
typedDelete' config tbl r = unsafeTypedDelete $ deleteSQL config tbl r

-- | Make typed 'Delete' from 'Table' and 'Restriction'.
typedDelete :: Table r -> Restriction p r -> Delete p
typedDelete =  typedDelete' defaultConfig

restrictedTable :: TableDerivable r => Restriction p r -> Table r
restrictedTable =  const derivedTable

-- | Make typed 'Delete' from 'Config', derived table and 'RestrictContext'
derivedDelete' :: TableDerivable r => Config -> RestrictedStatement r (PlaceHolders p) -> Delete p
derivedDelete' config rc = typedDelete' config (restrictedTable rs) rs  where
  rs = restriction' rc

-- | Make typed 'Delete' from 'defaultConfig', derived table and 'RestrictContext'
derivedDelete :: TableDerivable r => RestrictedStatement r (PlaceHolders p) -> Delete p
derivedDelete = derivedDelete' defaultConfig

-- | Show delete SQL string
instance Show (Delete p) where
  show = untypeDelete


-- | Untype interface for typed no-result type statments
--   with single type parameter which represents place-holder parameter 'p'.
class UntypeableNoFetch s where
  untypeNoFetch :: s p -> String

instance UntypeableNoFetch Insert where
  untypeNoFetch = untypeInsert

instance UntypeableNoFetch InsertQuery where
  untypeNoFetch = untypeInsertQuery

instance UntypeableNoFetch Update where
  untypeNoFetch = untypeUpdate

instance UntypeableNoFetch Delete where
  untypeNoFetch = untypeDelete

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Database.Relational.Type
-- Copyright   : 2013-2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines typed SQL.
module Database.Relational.Type (
  -- * Typed query statement
  Query (..), unsafeTypedQuery,

  relationalQuery_,
  relationalQuery', relationalQuery,

  relationalQuerySQL,

  -- * Typed update statement
  KeyUpdate (..), unsafeTypedKeyUpdate, typedKeyUpdate, typedKeyUpdateTable, keyUpdate,
  Update (..), unsafeTypedUpdate, typedUpdate', update', update, updateNoPH,
  typedUpdateAllColumn, updateAllColumn', updateAllColumn, updateAllColumnNoPH,

  updateSQL,

  -- * Typed insert statement
  Insert (..), untypeChunkInsert, chunkSizeOfInsert,
  unsafeTypedInsert', unsafeTypedInsert, typedInsert', insert,
  typedInsertValue', insertValue', insertValue, insertValueNoPH,
  insertValueList', insertValueList,
  InsertQuery (..), unsafeTypedInsertQuery,
  typedInsertQuery', insertQuery', insertQuery,

  insertQuerySQL,

  -- * Typed delete statement
  Delete (..), unsafeTypedDelete, typedDelete', delete', delete, deleteNoPH,

  deleteSQL,

  -- * Generalized interfaces
  UntypeableNoFetch (..),

  -- * Deprecated

  typedUpdate,
  typedInsert, typedInsertValue, typedInsertQuery,
  typedDelete,

  derivedKeyUpdate,
  derivedUpdate', derivedUpdate,
  derivedUpdateAllColumn', derivedUpdateAllColumn,

  derivedInsert,
  derivedInsertValue', derivedInsertValue,
  derivedInsertQuery,

  derivedDelete', derivedDelete,
  ) where

import Control.Applicative ((<$>))
#if MIN_VERSION_base(4, 7, 0)
import Data.Coerce (coerce)
#else
import Unsafe.Coerce (unsafeCoerce)
#endif
import Data.DList (fromList)
import Data.Monoid ((<>), mconcat)

import Language.SQL.Keyword (Keyword)
import Database.Record (PersistableWidth, PersistableRecordWidth, derivedWidth)

import Database.Relational.Internal.Config (Config, defaultConfig)
import Database.Relational.Internal.ContextType (Flat)
import Database.Relational.Internal.String (showStringSQL)
import Database.Relational.SqlSyntax (Record, SQLWithPlaceholderOffsets, withPlaceholderOffsets, detachPlaceholderOffsets)

import Database.Relational.Monad.BaseType (Relation, sqlFromRelationWith, defaultPlaceholders)
import Database.Relational.Monad.Restrict (Restrict)
import Database.Relational.Monad.Assign (Assign)
import Database.Relational.Monad.Register (Register)
import Database.Relational.Relation (tableOf)
import Database.Relational.Effect
  (liftTargetAllColumn', InsertTarget, insertTarget',
   deleteFromRestriction, updateFromUpdateTarget, piRegister,
   sqlChunkFromInsertTarget, sqlFromInsertTarget, sqlChunksFromRecordList)
import Database.Relational.Pi (Pi)
import Database.Relational.Table (Table, TableDerivable, derivedTable)
import Database.Relational.ProjectableClass (LiteralSQL)
import Database.Relational.SimpleSql
  (insertPrefixSQL, updateOtherThanKeySQL, )
import Database.Relational.Monad.Trans.ReadPlaceholders (ReadPlaceholders,)


-- | Query type with place-holder parameter 'p' and query result type 'a'.
newtype Query p a = Query { untypeQuery :: SQLWithPlaceholderOffsets }

-- | Unsafely make typed 'Query' from SQL string.
unsafeTypedQuery :: SQLWithPlaceholderOffsets    -- ^ Query SQL to type
                 -> Query p a -- ^ Typed result
unsafeTypedQuery =  Query

-- | Show query SQL string
instance Show (Query p a) where
  show = detachPlaceholderOffsets . untypeQuery

-- | From 'Relation' into untyped SQL query string.
relationalQuerySQL :: PersistableWidth p => Config -> Relation p r -> [Keyword] -> SQLWithPlaceholderOffsets
relationalQuerySQL config rel qsuf =
  showStringSQL . (<> mconcat qsuf) <$> sqlFromRelationWith rel defaultPlaceholders config

-- | From 'Relation' into typed 'Query' with suffix SQL words.
relationalQuery_ :: PersistableWidth p
                 => Config
                 -> Relation p r  -- ^ relation to finalize building
                 -> [Keyword]     -- ^ suffix SQL words. for example, `[FOR, UPDATE]`, `[FETCH, FIRST, "3", ROWS, ONLY]` ...
                 -> Query p r     -- ^ finalized query
relationalQuery_ config rel qsuf =
  unsafeTypedQuery $ relationalQuerySQL config rel qsuf

-- | From 'Relation' into typed 'Query' with suffix SQL words.
relationalQuery' :: PersistableWidth p
                 => Relation p r  -- ^ relation to finalize building
                 -> [Keyword]     -- ^ suffix SQL words. for example, `[FOR, UPDATE]`, `[FETCH, FIRST, "3", ROWS, ONLY]` ...
                 -> Query p r     -- ^ finalized query
relationalQuery' = relationalQuery_ defaultConfig

-- | From 'Relation' into typed 'Query'.
relationalQuery :: PersistableWidth p
                => Relation p r  -- ^ relation to finalize building
                -> Query p r     -- ^ finalized query
relationalQuery =  (`relationalQuery'` [])


-- | Update type with key type 'p' and update record type 'a'.
--   Columns to update are record columns other than key columns,
--   So place-holder parameter type is the same as record type 'a'.
data KeyUpdate p a = KeyUpdate { updateKey :: Pi a p
                               , untypeKeyUpdate :: SQLWithPlaceholderOffsets
                               }

-- | Unsafely make typed 'KeyUpdate' from SQL string.
unsafeTypedKeyUpdate :: Pi a p -> SQLWithPlaceholderOffsets -> KeyUpdate p a
unsafeTypedKeyUpdate =  KeyUpdate

-- | Make typed 'KeyUpdate' from 'Table' and key columns selector 'Pi'.
typedKeyUpdate :: forall a p. PersistableWidth p => Table a -> Pi a p -> KeyUpdate p a
typedKeyUpdate tbl key =
    unsafeTypedKeyUpdate key . withPlaceholderOffsets phs $ updateOtherThanKeySQL tbl key
  where
    phs = fromList [0 .. (snd (derivedWidth :: (PersistableRecordWidth p, Int)) - 1)]

-- | Make typed 'KeyUpdate' object using derived info specified by 'Relation' type.
typedKeyUpdateTable :: (PersistableWidth p, TableDerivable r) => Relation () r -> Pi r p -> KeyUpdate p r
typedKeyUpdateTable =  typedKeyUpdate . tableOf

-- keyUpdate'
-- Config parameter is not yet required for KeyUpdate.

-- | Make typed 'KeyUpdate' from derived table and key columns selector 'Pi'.
keyUpdate :: (PersistableWidth p, TableDerivable r) => Pi r p -> KeyUpdate p r
keyUpdate = typedKeyUpdate derivedTable

{-# DEPRECATED derivedKeyUpdate "use keyUpdate instead of this." #-}
-- | Make typed 'KeyUpdate' from derived table and key columns selector 'Pi'.
derivedKeyUpdate :: (PersistableWidth p, TableDerivable r) => Pi r p -> KeyUpdate p r
derivedKeyUpdate = keyUpdate

-- | Show update SQL string
instance Show (KeyUpdate p a) where
  show = detachPlaceholderOffsets . untypeKeyUpdate


-- | Update type with place-holder parameter 'p'.
newtype Update p = Update { untypeUpdate :: SQLWithPlaceholderOffsets }

-- | Unsafely make typed 'Update' from SQL string.
unsafeTypedUpdate :: SQLWithPlaceholderOffsets -> Update p
unsafeTypedUpdate =  Update

-- | Make untyped update SQL string from 'Table' and 'Assign' computation.
updateSQL :: PersistableWidth p => Config -> Table r -> (Record Flat r -> ReadPlaceholders p (Assign r) ()) -> SQLWithPlaceholderOffsets
updateSQL config tbl ut = showStringSQL <$> updateFromUpdateTarget config tbl ut

-- | Make typed 'Update' from 'Config', 'Table' and 'Assign' computation.
typedUpdate' :: PersistableWidth p => Config -> Table r -> (Record Flat r -> ReadPlaceholders p (Assign r) ()) -> Update p
typedUpdate' config tbl ut = unsafeTypedUpdate $ updateSQL config tbl ut

{-# DEPRECATED typedUpdate "use `typedUpdate' defaultConfig` instead of this." #-}
-- | Make typed 'Update' using 'defaultConfig', 'Table' and 'Assign' computation.
typedUpdate :: PersistableWidth p => Table r -> (Record Flat r -> ReadPlaceholders p (Assign r) ()) -> Update p
typedUpdate =  typedUpdate' defaultConfig

targetTable :: TableDerivable r => (Record Flat r -> ReadPlaceholders p (Assign r) ()) -> Table r
targetTable =  const derivedTable

-- | Make typed 'Update' from 'Config', derived table and 'Assign' computation.
update' :: PersistableWidth p => TableDerivable r => Config -> (Record Flat r -> ReadPlaceholders p (Assign r) ()) -> Update p
update' config ac =  typedUpdate' config (targetTable ac) ac

{-# DEPRECATED derivedUpdate' "use `update'` instead of this." #-}
-- | Make typed 'Update' from 'Config', derived table and 'Assign' computation.
derivedUpdate' :: (PersistableWidth p, TableDerivable r) => Config -> (Record Flat r -> ReadPlaceholders p (Assign r) ()) -> Update p
derivedUpdate' = update'

-- | Make typed 'Update' from 'defaultConfig', derived table and 'Assign' computation.
update :: (PersistableWidth p, TableDerivable r) => (Record Flat r -> ReadPlaceholders p (Assign r) ()) -> Update p
update = update' defaultConfig

-- | Make typed 'Update' from 'defaultConfig', derived table and 'Assign' computation with no(unit) placeholder.
updateNoPH :: TableDerivable r => (Record Flat r -> ReadPlaceholders () (Assign r) ()) -> Update ()
updateNoPH = update

{-# DEPRECATED derivedUpdate "use `update` instead of this." #-}
-- | Make typed 'Update' from 'defaultConfig', derived table and 'Assign' computation.
derivedUpdate :: (PersistableWidth p, TableDerivable r) => (Record Flat r -> ReadPlaceholders p (Assign r) ()) -> Update p
derivedUpdate = update


-- | Make typed 'Update' from 'Config', 'Table' and 'Restrict' computation.
--   Update target is all column.
typedUpdateAllColumn' :: (PersistableWidth p, PersistableWidth r)
                      => Config
                      -> Table r
                      -> (Record Flat r -> ReadPlaceholders p Restrict ())
                      -> Update (r, p)
typedUpdateAllColumn' config tbl r = typedUpdate' config tbl $ liftTargetAllColumn' r

-- | Make typed 'Update' from 'Table' and 'Restrict' computation.
--   Update target is all column.
typedUpdateAllColumn :: (PersistableWidth p, PersistableWidth r)
                     => Table r
                     -> (Record Flat r -> ReadPlaceholders p Restrict ())
                     -> Update (r, p)
typedUpdateAllColumn = typedUpdateAllColumn' defaultConfig

-- | Make typed 'Update' from 'Config', derived table and 'Restrict' computation.
--   Update target is all column.
updateAllColumn' :: (PersistableWidth p, PersistableWidth r, TableDerivable r)
                 => Config
                 -> (Record Flat r -> ReadPlaceholders p Restrict ())
                 -> Update (r, p)
updateAllColumn' config = typedUpdateAllColumn' config derivedTable

{-# DEPRECATED derivedUpdateAllColumn' "use `updateAllColumn'` instead of this." #-}
-- | Deprecated. use 'updateAllColumn''.
derivedUpdateAllColumn' :: (PersistableWidth p, PersistableWidth r, TableDerivable r)
                        => Config
                        -> (Record Flat r -> ReadPlaceholders p Restrict ())
                        -> Update (r, p)
derivedUpdateAllColumn' = updateAllColumn'

-- | Make typed 'Update' from 'defaultConfig', derived table and 'Restrict' computation.
--   Update target is all column.
updateAllColumn :: (PersistableWidth p, PersistableWidth r, TableDerivable r)
                => (Record Flat r -> ReadPlaceholders p Restrict ())
                -> Update (r, p)
updateAllColumn = updateAllColumn' defaultConfig

-- | Make typed 'Update' from 'defaultConfig', derived table and 'Restrict' computation
--   without placeholder other than target table columns.
--   Update target is all column.
updateAllColumnNoPH :: (PersistableWidth r, TableDerivable r)
                    => (Record Flat r -> ReadPlaceholders () Restrict ())
                    -> Update r
updateAllColumnNoPH =
  coerce . typedUpdate' defaultConfig derivedTable . liftTargetAllColumn'
#if !(MIN_VERSION_base(4, 7, 0))
 where
   coerce :: a -> b
   coerce = unsafeCoerce
#endif

{-# DEPRECATED derivedUpdateAllColumn "use `updateAllColumn` instead of this." #-}
-- | Deprecated. use 'updateAllColumn'.
derivedUpdateAllColumn :: (PersistableWidth p, PersistableWidth r, TableDerivable r)
                       => (Record Flat r -> ReadPlaceholders p Restrict ())
                       -> Update (r, p)
derivedUpdateAllColumn = updateAllColumn

-- | Show update SQL string
instance Show (Update p) where
  show = detachPlaceholderOffsets . untypeUpdate


-- | Insert type to insert record type 'a'.
data Insert a   =
  Insert
  { untypeInsert  :: SQLWithPlaceholderOffsets
  , chunkedInsert :: Maybe (SQLWithPlaceholderOffsets, Int)
  }

-- | Statement to use chunked insert
untypeChunkInsert :: Insert a -> SQLWithPlaceholderOffsets
untypeChunkInsert ins = maybe (untypeInsert ins) fst $ chunkedInsert ins

-- | Size to use chunked insert
chunkSizeOfInsert :: Insert a -> Int
chunkSizeOfInsert = maybe 1 snd . chunkedInsert

-- | Unsafely make typed 'Insert' from single insert and chunked insert SQL.
unsafeTypedInsert' :: SQLWithPlaceholderOffsets -> SQLWithPlaceholderOffsets -> Int -> Insert a
unsafeTypedInsert' s = curry (Insert s . Just)

-- | Unsafely make typed 'Insert' from single insert SQL.
unsafeTypedInsert :: SQLWithPlaceholderOffsets -> Insert a
unsafeTypedInsert s = Insert s Nothing

-- | Make typed 'Insert' from 'Table' and columns selector 'Pi' with configuration parameter.
typedInsert' :: (PersistableWidth r', TableDerivable r) => Config -> Table r -> Pi r r' -> Insert r'
typedInsert' config tbl =
  typedInsertValue' config tbl . insertTarget' . piRegister

{-# DEPRECATED typedInsert "use `typedInsert' defaultConfig` instead of this." #-}
-- | Make typed 'Insert' from 'Table' and columns selector 'Pi'.
typedInsert :: (PersistableWidth r', TableDerivable r) => Table r -> Pi r r' -> Insert r'
typedInsert =  typedInsert' defaultConfig

-- | Table type inferred 'Insert'.
insert :: (PersistableWidth r', PersistableWidth r, TableDerivable r) => Pi r r' -> Insert r'
insert = typedInsert' defaultConfig derivedTable

{-# DEPRECATED derivedInsert "use `insert` instead of this." #-}
-- | Table type inferred 'Insert'.
derivedInsert :: (PersistableWidth r', PersistableWidth r, TableDerivable r) => Pi r r' -> Insert r'
derivedInsert = insert

-- | Make typed 'Insert' from 'Config', 'Table' and monadic builded 'InsertTarget' object.
typedInsertValue' :: PersistableWidth p => Config -> Table r -> InsertTarget p r -> Insert p
typedInsertValue' config tbl it =
    unsafeTypedInsert'
    (showStringSQL <$> sqlFromInsertTarget config defaultPlaceholders tbl it)
    (showStringSQL <$> ci) n
  where
    (ci, n) = sqlChunkFromInsertTarget config defaultPlaceholders tbl it

{-# DEPRECATED typedInsertValue "use `typedInsertValue' defaultConfig` instead of this." #-}
-- | Make typed 'Insert' from 'Table' and monadic builded 'InsertTarget' object.
typedInsertValue :: PersistableWidth p => Table r -> InsertTarget p r -> Insert p
typedInsertValue = typedInsertValue' defaultConfig

-- | Make typed 'Insert' from 'Config', derived table and monadic builded 'Register' object.
insertValue' :: PersistableWidth p => TableDerivable r => Config -> ReadPlaceholders p (Register r) () -> Insert p
insertValue' config rs = typedInsertValue' config (rt rs) $ insertTarget' rs
  where
    rt :: TableDerivable r => ReadPlaceholders p (Register r) () -> Table r
    rt =  const derivedTable

{-# DEPRECATED derivedInsertValue' "use `insertValue'` instead of this." #-}
-- | Make typed 'Insert' from 'Config', derived table and monadic builded 'Register' object.
derivedInsertValue' :: PersistableWidth p => TableDerivable r => Config -> ReadPlaceholders p (Register r) () -> Insert p
derivedInsertValue' = insertValue'

-- | Make typed 'Insert' from 'defaultConfig', derived table and monadic builded 'Register' object.
insertValue :: (PersistableWidth p, TableDerivable r) => ReadPlaceholders p (Register r) () -> Insert p
insertValue = insertValue' defaultConfig

-- | Make typed 'Insert' from 'defaultConfig', derived table and monadic builded 'Register' object with no(unit) placeholder.
insertValueNoPH :: TableDerivable r => ReadPlaceholders () (Register r) () -> Insert ()
insertValueNoPH = insertValue

{-# DEPRECATED derivedInsertValue "use `insertValue` instead of this." #-}
-- | Make typed 'Insert' from 'defaultConfig', derived table and monadic builded 'Register' object.
derivedInsertValue :: (PersistableWidth p, TableDerivable r) => ReadPlaceholders p (Register r) () -> Insert p
derivedInsertValue = insertValue

-- | Make typed 'Insert' list from 'Config' and records list.
insertValueList' :: (TableDerivable r, LiteralSQL r')
                 => Config
                 -> Pi r r'
                 -> [r']
                 -> [Insert ()]
insertValueList' config pi' =
  map (unsafeTypedInsert . fmap showStringSQL)
  . sqlChunksFromRecordList config derivedTable pi'

-- | Make typed 'Insert' list from records list.
insertValueList :: (TableDerivable r, LiteralSQL r')
                => Pi r r'
                -> [r']
                -> [Insert ()]
insertValueList = insertValueList' defaultConfig

-- | Show insert SQL string.
instance Show (Insert a) where
  show = detachPlaceholderOffsets . untypeInsert

-- | InsertQuery type.
newtype InsertQuery p = InsertQuery { untypeInsertQuery :: SQLWithPlaceholderOffsets }

-- | Unsafely make typed 'InsertQuery' from SQL string.
unsafeTypedInsertQuery :: SQLWithPlaceholderOffsets -> InsertQuery p
unsafeTypedInsertQuery =  InsertQuery

-- | Make untyped insert select SQL string from 'Table', 'Pi' and 'Relation'.
insertQuerySQL :: PersistableWidth p => Config -> Table r -> Pi r r' -> Relation p r' -> SQLWithPlaceholderOffsets
insertQuerySQL config tbl pi' rel = showStringSQL . (insertPrefixSQL pi' tbl <>) <$> sqlFromRelationWith rel defaultPlaceholders config

-- | Make typed 'InsertQuery' from columns selector 'Table', 'Pi' and 'Relation' with configuration parameter.
typedInsertQuery' :: PersistableWidth p => Config -> Table r -> Pi r r' -> Relation p r' -> InsertQuery p
typedInsertQuery' config tbl pi' = unsafeTypedInsertQuery . insertQuerySQL config tbl pi'

{-# DEPRECATED typedInsertQuery "use `typedInsertQuery' defaultConfig` instead of this." #-}
-- | Make typed 'InsertQuery' from columns selector 'Table', 'Pi' and 'Relation'.
typedInsertQuery :: PersistableWidth p => Table r -> Pi r r' -> Relation p r' -> InsertQuery p
typedInsertQuery = typedInsertQuery' defaultConfig

-- | Table type inferred 'InsertQuery'.
insertQuery' :: (PersistableWidth p, TableDerivable r) => Config -> Pi r r' -> Relation p r' -> InsertQuery p
insertQuery' config = typedInsertQuery' config derivedTable

-- | Table type inferred 'InsertQuery' with 'defaultConfig'.
insertQuery :: (PersistableWidth p, TableDerivable r) => Pi r r' -> Relation p r' -> InsertQuery p
insertQuery = insertQuery' defaultConfig

{-# DEPRECATED derivedInsertQuery "use `insertQuery` instead of this." #-}
-- | Table type inferred 'InsertQuery'.
derivedInsertQuery :: (PersistableWidth p, TableDerivable r) => Pi r r' -> Relation p r' -> InsertQuery p
derivedInsertQuery = insertQuery

-- | Show insert SQL string.
instance Show (InsertQuery p) where
  show = detachPlaceholderOffsets . untypeInsertQuery


-- | Delete type with place-holder parameter 'p'.
newtype Delete p = Delete { untypeDelete :: SQLWithPlaceholderOffsets }

-- | Unsafely make typed 'Delete' from SQL string.
unsafeTypedDelete :: SQLWithPlaceholderOffsets -> Delete p
unsafeTypedDelete =  Delete

-- | Make untyped delete SQL string from 'Table' and 'Restrict' computation.
deleteSQL :: PersistableWidth p => Config -> Table r -> (Record Flat r -> ReadPlaceholders p Restrict ()) -> SQLWithPlaceholderOffsets
deleteSQL config tbl r = showStringSQL <$> deleteFromRestriction config tbl r

-- | Make typed 'Delete' from 'Config', 'Table' and 'Restrict' computation.
typedDelete' :: PersistableWidth p => Config -> Table r -> (Record Flat r -> ReadPlaceholders p Restrict ()) -> Delete p
typedDelete' config tbl r = unsafeTypedDelete $ deleteSQL config tbl r

{-# DEPRECATED typedDelete "use `typedDelete' defaultConfig` instead of this." #-}
-- | Make typed 'Delete' from 'Table' and 'Restrict' computation.
typedDelete :: PersistableWidth p => Table r -> (Record Flat r -> ReadPlaceholders p Restrict ()) -> Delete p
typedDelete =  typedDelete' defaultConfig

restrictedTable :: TableDerivable r => (Record Flat r -> ReadPlaceholders p Restrict ()) -> Table r
restrictedTable =  const derivedTable

-- | Make typed 'Delete' from 'Config', derived table and 'Restrict' computation.
delete' :: PersistableWidth p => TableDerivable r => Config -> (Record Flat r -> ReadPlaceholders p Restrict ()) -> Delete p
delete' config rc = typedDelete' config (restrictedTable rc) rc

{-# DEPRECATED derivedDelete' "use `delete'` instead of this." #-}
-- | Make typed 'Delete' from 'Config', derived table and 'Restrict' computation.
derivedDelete' :: (PersistableWidth p, TableDerivable r) => Config -> (Record Flat r -> ReadPlaceholders p Restrict ()) -> Delete p
derivedDelete' = delete'

-- | Make typed 'Delete' from 'defaultConfig', derived table and 'Restrict' computation.
delete :: (PersistableWidth p, TableDerivable r) => (Record Flat r -> ReadPlaceholders p Restrict ()) -> Delete p
delete = delete' defaultConfig

-- | Make typed 'Delete' from 'defaultConfig', derived table and 'Restrict' computation with no(unit) placeholder.
deleteNoPH :: TableDerivable r => (Record Flat r -> ReadPlaceholders () Restrict ()) -> Delete ()
deleteNoPH = delete

{-# DEPRECATED derivedDelete "use `delete` instead of this." #-}
-- | Make typed 'Delete' from 'defaultConfig', derived table and 'Restrict' computation.
derivedDelete :: (PersistableWidth p, TableDerivable r) => (Record Flat r -> ReadPlaceholders p Restrict ()) -> Delete p
derivedDelete = delete

-- | Show delete SQL string
instance Show (Delete p) where
  show = detachPlaceholderOffsets . untypeDelete


-- | Untype interface for typed no-result type statments
--   with single type parameter which represents place-holder parameter 'p'.
class UntypeableNoFetch s where
  untypeNoFetch :: s p -> SQLWithPlaceholderOffsets

instance UntypeableNoFetch Insert where
  untypeNoFetch = untypeInsert

instance UntypeableNoFetch InsertQuery where
  untypeNoFetch = untypeInsertQuery

instance UntypeableNoFetch Update where
  untypeNoFetch = untypeUpdate

instance UntypeableNoFetch Delete where
  untypeNoFetch = untypeDelete

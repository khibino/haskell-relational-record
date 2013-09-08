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

  relationalQuery,

  -- * Typed update statement
  KeyUpdate (..), unsafeTypedKeyUpdate, typedKeyUpdate,
  Update (..), unsafeTypedUpdate, typedUpdate, targetUpdate,
  typedUpdateAllColumn, restricredUpdateAllColumn,

  -- * Typed insert statement
  Insert (..), unsafeTypedInsert, typedInsert,

  -- * Typed delete statement
  Delete (..), unsafeTypedDelete, typedDelete, restrictedDelete
  ) where

import Database.Record (PersistableWidth)

import Database.Relational.Query.Relation (Relation, sqlFromRelation)
import Database.Relational.Query.Restriction
  (Restriction, RestrictionContext, restriction',
   UpdateTarget, UpdateTargetContext, updateTarget', liftTargetAllColumn',
   sqlWhereFromRestriction, sqlFromUpdateTarget)
import Database.Relational.Query.Pi (Pi)
import Database.Relational.Query.Table (Table)
import Database.Relational.Query.SQL
  (updateOtherThanKeySQL, insertSQL, updatePrefixSQL, deleteSQL)


-- | Query type with place-holder parameter 'p' and query result type 'a'.
newtype Query p a = Query { untypeQuery :: String }

-- | Unsafely make typed 'Query' from SQL string.
unsafeTypedQuery :: String    -- ^ Query SQL to type
                 -> Query p a -- ^ Typed result
unsafeTypedQuery =  Query

-- | Show query SQL string
instance Show (Query p a) where
  show = untypeQuery

-- | From 'Relation' into typed 'Query'.
relationalQuery :: Relation p r -> Query p r
relationalQuery =  unsafeTypedQuery . sqlFromRelation


-- | Update type with key type 'p' and update record type 'a'.
--   Columns to update are record columns other than key columns,
--   So all place-holder correspond to record type 'a' columns.
data KeyUpdate p a = KeyUpdate { updateKey :: Pi a p
                               , untypeKeyUpdate :: String
                               }

-- | Unsafely make typed 'KeyUpdate' from SQL string.
unsafeTypedKeyUpdate :: Pi a p -> String -> KeyUpdate p a
unsafeTypedKeyUpdate =  KeyUpdate

-- | Make typed 'KeyUpdate' from 'Table' and key indexes.
typedKeyUpdate :: Table a -> Pi a p -> KeyUpdate p a
typedKeyUpdate tbl key = unsafeTypedKeyUpdate key $ updateOtherThanKeySQL tbl key

-- | Show update SQL string
instance Show (KeyUpdate p a) where
  show = untypeKeyUpdate


-- | Update type with place-holder parameter 'p'.
newtype Update p = Update { untypeUpdate :: String }

-- | Unsafely make typed 'Update' from SQL string.
unsafeTypedUpdate :: String -> Update p
unsafeTypedUpdate =  Update

-- | Make typed 'Update' from 'Table' and 'Restriction'.
typedUpdate :: Table r -> UpdateTarget p r -> Update p
typedUpdate tbl ut = unsafeTypedUpdate . updatePrefixSQL tbl
                     . sqlFromUpdateTarget tbl ut $ ""

-- | Directly make typed 'Update' from 'Table' and 'Target' monad context.
targetUpdate :: Table r
             -> UpdateTargetContext p r -- ^ 'Target' monad context
             -> Update p
targetUpdate tbl = typedUpdate tbl . updateTarget'

-- | Make typed 'Update' from 'Table' and 'Restriction'.
--   Update target is all column.
typedUpdateAllColumn :: PersistableWidth r
                     => Table r
                     -> Restriction p r
                     -> Update (r, p)
typedUpdateAllColumn tbl r = typedUpdate tbl $ liftTargetAllColumn' r

-- | Directly make typed 'Update' from 'Table' and 'Restrict' monad context.
--   Update target is all column.
restricredUpdateAllColumn :: PersistableWidth r
                           => Table r
                           -> RestrictionContext p r
                           -> Update (r, p)
restricredUpdateAllColumn tbl = typedUpdateAllColumn tbl . restriction'

-- | Show update SQL string
instance Show (Update p) where
  show = untypeUpdate


-- | Insert type to insert record type 'a'.
newtype Insert a   = Insert { untypeInsert :: String }

-- | Unsafely make typed 'Insert' from SQL string.
unsafeTypedInsert :: String -> Insert a
unsafeTypedInsert =  Insert

-- | Make typed 'Insert' from 'Table'
typedInsert :: Table r -> Insert r
typedInsert =  unsafeTypedInsert . insertSQL

-- | Show insert SQL string
instance Show (Insert a) where
  show = untypeInsert


-- | Delete type with place-holder parameter 'p'.
newtype Delete p = Delete { untypeDelete :: String }

-- | Unsafely make typed 'Delete' from SQL string.
unsafeTypedDelete :: String -> Delete p
unsafeTypedDelete =  Delete

-- | Make typed 'Delete' from 'Table' and 'Restriction'.
typedDelete :: Table r -> Restriction p r -> Delete p
typedDelete tbl r = unsafeTypedDelete . deleteSQL tbl
                    . sqlWhereFromRestriction tbl r $ ""

-- | Directly make typed 'Delete' from 'Table' and 'Restrict' monad context.
restrictedDelete :: Table r
                 -> RestrictionContext p r -- ^ 'Restrict' monad context.
                 -> Delete p
restrictedDelete tbl = typedDelete tbl . restriction'

-- | Show delete SQL string
instance Show (Delete p) where
  show = untypeDelete

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
  Query (untypeQuery), unsafeTypedQuery,

  relationalQuery,
  fromRelation,

  -- * Typed update statement
  KeyUpdate(untypeKeyUpdate), unsafeTypedKeyUpdate, typedKeyUpdate,
  Update(untypeUpdate), unsafeTypedUpdate, restrictedUpdate,

  -- * Typed insert statement
  Insert(untypeInsert), unsafeTypedInsert, typedInsert,

  -- * Typed delete statement
  Delete(untypeDelete), unsafeTypedDelete, restrictedDelete
  ) where

import Database.Relational.Query.Relation (Relation, sqlFromRelation)
import Database.Relational.Query.Restriction (Restriction, sqlWhereFromRestriction)
import Database.Relational.Query.Table (Table)
import Database.Relational.Query.SQL (updateSQL, insertSQL, updateAllColumnsSQL, deleteSQL)


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

{-# DEPRECATED fromRelation "Use relationalQuery instead of this." #-}
-- | From 'Relation' into typed 'Query'.
fromRelation :: Relation p r -> Query p r
fromRelation =  relationalQuery


-- | Update type with place-holder parameter 'p' and update record type 'a'.
newtype KeyUpdate p a = KeyUpdate { untypeKeyUpdate :: String }

-- | Unsafely make typed 'KeyUpdate' from SQL string.
unsafeTypedKeyUpdate :: String -> KeyUpdate p a
unsafeTypedKeyUpdate =  KeyUpdate

-- | Make typed 'KeyUpdate' from 'Table' and key indexes.
typedKeyUpdate :: Table r -> [Int] -> KeyUpdate p r
typedKeyUpdate tbl = unsafeTypedKeyUpdate . updateSQL tbl

-- | Show update SQL string
instance Show (KeyUpdate p a) where
  show = untypeKeyUpdate


-- | Update type with place-holder parameter 'p' and update record type 'a'.
newtype Update p a = Update { untypeUpdate :: String }

-- | Unsafely make typed 'Update' from SQL string.
unsafeTypedUpdate :: String -> Update p a
unsafeTypedUpdate =  Update

-- | Make typed 'Update' from 'Table' and 'Restriction'.
restrictedUpdate :: Table r -> Restriction p r -> Update p r
restrictedUpdate tbl r = unsafeTypedUpdate . updateAllColumnsSQL tbl
                         . sqlWhereFromRestriction tbl r $ ""

-- | Show update SQL string
instance Show (Update p a) where
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
restrictedDelete :: Table r -> Restriction p r -> Delete p
restrictedDelete tbl r = unsafeTypedDelete . deleteSQL tbl
                         . sqlWhereFromRestriction tbl r $ ""

-- | Show delete SQL string
instance Show (Delete p) where
  show = untypeDelete

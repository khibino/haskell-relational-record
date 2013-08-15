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

  relationQuery,
  fromRelation,

  -- * Typed update statement
  Update(untypeUpdate), unsafeTypedUpdate, typedUpdate,

  restrictedUpdate,

  -- * Typed insert statement
  Insert(untypeInsert), unsafeTypedInsert, typedInsert,

  -- * Typed delete statement
  Delete(untypeDelete), unsafeTypedDelete,

  restrictedDelete
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
relationQuery :: Relation p r -> Query p r
relationQuery =  unsafeTypedQuery . sqlFromRelation

{-# DEPRECATED fromRelation "Use relationQuery instead of this." #-}
-- | From 'Relation' into typed 'Query'.
fromRelation :: Relation p r -> Query p r
fromRelation =  relationQuery


-- | Update type with place-holder parameter 'p' and update record type 'a'.
newtype Update p a = Update { untypeUpdate :: String }

-- | Unsafely make typed 'Update' from SQL string.
unsafeTypedUpdate :: String -> Update p a
unsafeTypedUpdate =  Update

-- | Make typed 'Update' from 'Table' and key indexes.
typedUpdate :: Table r -> [Int] -> Update p r
typedUpdate tbl = unsafeTypedUpdate . updateSQL tbl

-- | Show update SQL string
instance Show (Update p a) where
  show = untypeUpdate

-- | Make typed 'Update' from 'Table' and 'Restriction'.
restrictedUpdate :: Table r -> Restriction p r -> Update p r
restrictedUpdate tbl r = unsafeTypedUpdate . updateAllColumnsSQL tbl
                         . sqlWhereFromRestriction tbl r $ ""


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


-- | Delete type with place-holder parameter 'p' and delete record type 'a'.
newtype Delete p a = Delete { untypeDelete :: String }

-- | Unsafely make typed 'Delete' from SQL string.
unsafeTypedDelete :: String -> Delete p a
unsafeTypedDelete =  Delete

-- | Show delete SQL string
instance Show (Delete p a) where
  show = untypeDelete

-- | Make typed 'Delete' from 'Table' and 'Restriction'.
restrictedDelete :: Table r -> Restriction p r -> Delete p a
restrictedDelete tbl r = unsafeTypedDelete . deleteSQL tbl
                         . sqlWhereFromRestriction tbl r $ ""

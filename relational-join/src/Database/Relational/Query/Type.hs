-- |
-- Module      : Database.Relational.Query.Type
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines Query type with type parameter
-- which specifies place-holder parameter type and query result type.
module Database.Relational.Query.Type (
  Query (untypeQuery), unsafeTypedQuery, fromRelation,

  Update(untypeUpdate), unsafeTypedUpdate, typedUpdate,
  Insert(untypeInsert), unsafeTypedInsert, typedInsert
  ) where

import Database.Relational.Query.Relation (Relation, sqlFromRelation)
import Database.Relational.Query.Table (Table)
import Database.Relational.Query.SQL (updateSQL, insertSQL)


-- | Query type with place-holder parameter 'p' and query result type 'a'.
newtype Query p a = Query { untypeQuery :: String }

-- | Unsafely make typed 'Query' from SQL string.
unsafeTypedQuery :: String    -- ^ Query SQL to type
                 -> Query p a -- ^ Typed result
unsafeTypedQuery =  Query

-- | Show query SQL string
instance Show (Query p a) where
  show = untypeQuery

-- | From 'Relation' into type 'Query'.
fromRelation :: Relation p r -> Query p r
fromRelation =  unsafeTypedQuery . sqlFromRelation


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

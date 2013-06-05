
module Database.Relational.Query.Type (
  Query (untypeQuery), unsafeTypedQuery, fromRelation,

  Update(untypeUpdate), unsafeTypedUpdate, typedSingleKeyUpdate,
  Insert(untypeInsert), unsafeTypedInsert, typedInsert
  ) where

import Database.Relational.Query.Relation (Relation, sqlFromRelation)
import Database.Relational.Query.Table (Table)
import Database.Relational.Query.SQL (singleKeyUpdateSQL, insertSQL)


newtype Query p a = Query { untypeQuery :: String }

unsafeTypedQuery :: String -> Query p a
unsafeTypedQuery =  Query

instance Show (Query p a) where
  show = untypeQuery

fromRelation :: Relation p r -> Query p r
fromRelation =  unsafeTypedQuery . sqlFromRelation


newtype Update p a = Update { untypeUpdate :: String }

unsafeTypedUpdate :: String -> Update p a
unsafeTypedUpdate =  Update

typedSingleKeyUpdate :: Table r -> String -> Update p r
typedSingleKeyUpdate tbl =  unsafeTypedUpdate . singleKeyUpdateSQL tbl

instance Show (Update p a) where
  show = untypeUpdate


newtype Insert a   = Insert { untypeInsert :: String }

unsafeTypedInsert :: String -> Insert a
unsafeTypedInsert =  Insert

typedInsert :: Table r -> Insert r
typedInsert =  unsafeTypedInsert . insertSQL

instance Show (Insert a) where
  show = untypeInsert

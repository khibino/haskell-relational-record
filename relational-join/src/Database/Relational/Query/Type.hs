
module Database.Relational.Query.Type (
  Query (untypeQuery), unsafeTypedQuery, fromRelation,

  Update(untypeUpdate), unsafeTypedUpdate, typedSingleKeyUpdate,
  Insert(untypeInsert), unsafeTypedInsert, typedInsert
  ) where

import Database.Relational.Query.Relation (PrimeRelation)
import qualified Database.Relational.Query.Relation as Relation
import Database.Relational.Query.Table (Table)
import Database.Relational.Query.SQL (singleKeyUpdateSQL, insertSQL)


newtype Query p a = Query { untypeQuery :: String }

unsafeTypedQuery :: String -> Query p a
unsafeTypedQuery =  Query

instance Show (Query p a) where
  show = untypeQuery

fromRelation :: PrimeRelation p r -> Query p r
fromRelation =  unsafeTypedQuery . Relation.toSQL


newtype Update p a = Update { untypeUpdate :: String }

unsafeTypedUpdate :: String -> Update p a
unsafeTypedUpdate =  Update

typedSingleKeyUpdate :: Table r -> String -> Update p r
typedSingleKeyUpdate tbl =  unsafeTypedUpdate . singleKeyUpdateSQL tbl

newtype Insert a   = Insert { untypeInsert :: String }

unsafeTypedInsert :: String -> Insert a
unsafeTypedInsert =  Insert

typedInsert :: Table r -> Insert r
typedInsert =  unsafeTypedInsert . insertSQL

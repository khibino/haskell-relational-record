
module Database.Relational.Query.Type (
  Query (untypeQuery), unsafeTypedQuery, fromRelation
  ) where

import Database.Relational.Query.Relation (Relation)
import qualified Database.Relational.Query.Relation as Relation

newtype Query p a = Query { untypeQuery :: String }

unsafeTypedQuery :: String -> Query p a
unsafeTypedQuery =  Query

instance Show (Query p a) where
  show = untypeQuery


fromRelation :: Relation r -> Query p r
fromRelation =  unsafeTypedQuery . Relation.toSQL

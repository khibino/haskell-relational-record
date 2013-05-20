module Database.Relational.Query.Relation (

  Order (..),
  PrimeRelation, Relation,

  toMaybe,
  fromTable,

  toSubQuery,
  toSQL,

  finalizeRelation
  ) where

import Prelude hiding (product, and)
import Data.List (intercalate)

import Database.Relational.Query.Expr (Expr)

import Database.Relational.Query.Table (Table)
import qualified Database.Relational.Query.Table as Table

import Database.Relational.Query.Sub (SubQuery, subQuery)
import qualified Database.Relational.Query.Sub as SubQuery

import Database.Relational.Query.Product (Product)

import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection

import Language.SQL.Keyword (Keyword(..), unwordsSQL)
import qualified Language.SQL.Keyword as SQL

import Database.Relational.Query.Internal.Context (Order(..), composedSQL)


data PrimeRelation a r = Table (Table r)
                       | Relation
                         { projection  :: Projection r
                         , product     :: Product
                         , restriction :: Maybe (Expr Bool)
                         , orderByRev  :: [(Order, String)]
                         }

type Relation = PrimeRelation ()

toMaybe :: PrimeRelation a r -> PrimeRelation a (Maybe r)
toMaybe =  d  where
  d (Table t)                       = Table $ Table.toMaybe t
  d r@(Relation { projection = p }) = r { projection = Projection.toMaybe p }

width :: PrimeRelation a r -> Int
width =  d  where
  d (Table t)                      = Table.width t
  d (Relation { projection = p } ) = Projection.width p

fromTable :: Table r -> Relation r
fromTable =  Table

toSubQuery :: PrimeRelation a r -> SubQuery
toSubQuery =  d  where
  d (Table t)          = SubQuery.fromTable t
  d rel@(Relation { }) = subQuery
                         (composedSQL
                          (projection rel)
                          (product rel)
                          (restriction rel)
                          (orderByRev rel)
                         )
                         (width rel)

finalizeRelation :: Projection r -> Product -> Maybe (Expr Bool) -> [(Order, String)] -> PrimeRelation a r
finalizeRelation =  Relation

fromTableToSql :: Table r -> String
fromTableToSql t =
  unwordsSQL
  $ [SELECT, SQL.word $ ", " `intercalate` Table.columns t,
     FROM, SQL.word $ Table.name t]

toSQL :: PrimeRelation a r -> String
toSQL =  d  where
  d (Table t)           = fromTableToSql t
  d (rel@(Relation {})) = SubQuery.toSQL . toSubQuery $ rel

instance Show (PrimeRelation a r) where
  show = show . toSubQuery

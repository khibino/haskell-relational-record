
module Database.Relational.Query.Relation (

  Relation,

  outer,
  fromTable,

  toSubQuery,

  finalizeRelation
  ) where

import Prelude hiding (product, and)

import Database.Relational.Query.AliasId (asColumnN)

import Database.Relational.Query.Expr (UExpr, showExpr)

import Database.Relational.Query.Table (Table)
import qualified Database.Relational.Query.Table as Table

import Database.Relational.Query.Sub (SubQuery, subQuery)
import qualified Database.Relational.Query.Sub as SubQuery

import Database.Relational.Query.Product (Product, productSQL)

import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection

import Language.SQL.Keyword (Keyword(..), unwordsSQL)
import qualified Language.SQL.Keyword as SQL


data Relation r = Table (Table r)
                | Relation
                  { projection  :: Projection r
                  , product     :: Product
                  , restriction :: Maybe (UExpr Bool)
                  }

outer :: Relation r -> Relation (Maybe r)
outer =  d  where
  d (Table t)                       = Table $ Table.outer t
  d r@(Relation { projection = p }) = r { projection = Projection.outer p }

width :: Relation r -> Int
width =  d  where
  d (Table t)                      = Table.width t
  d (Relation { projection = p } ) = Projection.width p

fromTable :: Table r -> Relation r
fromTable =  Table

composedSQL :: Projection r -> Product -> Maybe (UExpr Bool) -> String
composedSQL pj pd re =
  unwordsSQL
  $ [SELECT, columns' `SQL.sepBy` SQL.word ", ",
     FROM, SQL.word . productSQL $ pd ]
  ++ wheres re
    where columns' = zipWith
                    (\f n -> SQL.word f `asColumnN` n)
                    (Projection.columns pj)
                    [(0 :: Int)..]
          wheres  = maybe [] (\e -> [WHERE, SQL.word . showExpr $ e])

toSubQuery :: Relation r -> SubQuery
toSubQuery =  d  where
  d (Table t)          = SubQuery.fromTable t
  d rel@(Relation { }) = subQuery
                         (composedSQL
                          (projection rel)
                          (product rel)
                          (restriction rel))
                         (width rel)

finalizeRelation :: Projection r -> Product -> Maybe (UExpr Bool) -> Relation r
finalizeRelation =  Relation

instance Show (Relation r) where
  show = show . toSubQuery

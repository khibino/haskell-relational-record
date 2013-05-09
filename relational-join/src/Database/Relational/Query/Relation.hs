{-# LANGUAGE OverloadedStrings #-}

module Database.Relational.Query.Relation (

  Order (..),
  Relation,

  outer,
  fromTable,

  toSubQuery,
  toSQL,

  finalizeRelation
  ) where

import Prelude hiding (product, and)
import Data.List (foldl')

import Database.Relational.Query.AliasId (asColumnN)

import Database.Relational.Query.Expr (Expr, showExpr)

import Database.Relational.Query.Table (Table)
import qualified Database.Relational.Query.Table as Table

import Database.Relational.Query.Sub (SubQuery, subQuery)
import qualified Database.Relational.Query.Sub as SubQuery

import Database.Relational.Query.Product (Product, productSQL)

import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection

import Language.SQL.Keyword (Keyword(..), unwordsSQL)
import qualified Language.SQL.Keyword as SQL


data Order = Asc | Desc

data Relation r = Table (Table r)
                | Relation
                  { projection  :: Projection r
                  , product     :: Product
                  , restriction :: Maybe (Expr Bool)
                  , orderByRev  :: [(Order, String)]
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

composedSQL :: Projection r -> Product -> Maybe (Expr Bool) -> [(Order, String)] -> String
composedSQL pj pd re odRev =
  unwordsSQL
  $ [SELECT, columns' `SQL.sepBy` ", ",
     FROM, SQL.word . productSQL $ pd]
  ++ wheres re
  ++ orders
    where columns' = zipWith
                    (\f n -> SQL.word f `asColumnN` n)
                    (Projection.columns pj)
                    [(0 :: Int)..]
          wheres  = maybe [] (\e -> [WHERE, SQL.word . showExpr $ e])
          order Asc  = ASC
          order Desc = DESC
          orderList = foldl' (\ r (o, e) -> [SQL.word e, order o] `SQL.sepBy` " "  : r) [] odRev
          orders | null odRev = []
                 | otherwise  = [ORDER, BY, orderList `SQL.sepBy` ", "]

toSubQuery :: Relation r -> SubQuery
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

finalizeRelation :: Projection r -> Product -> Maybe (Expr Bool) -> [(Order, String)] -> Relation r
finalizeRelation =  Relation

toSQL :: Relation r -> String
toSQL =  SubQuery.toSQL . toSubQuery

instance Show (Relation r) where
  show = show . toSubQuery

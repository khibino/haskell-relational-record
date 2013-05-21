{-# LANGUAGE OverloadedStrings #-}

module Database.Relational.Query.Internal.Context (
  Context, Order (..),

  primContext, currentAliasId, product, restriction, orderByRev,
  nextAliasContext,

  updateProduct,
  updateRestriction,
  updateOrderBy,

  composedSQL,
  composeSQL
  ) where

import Prelude hiding (product)
import Data.List (foldl')

import Database.Relational.Query.AliasId.Unsafe (primAlias)
import Database.Relational.Query.AliasId (AliasId, newAliasId, asColumnN)

import Database.Relational.Query.Expr (Expr, showExpr)

import Database.Relational.Query.Product (QueryProductNode, Product, productSQL)
import qualified Database.Relational.Query.Product as Product

import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import qualified Database.Relational.Query.Projectable as Projectable

import Language.SQL.Keyword (Keyword(..), unwordsSQL)
import qualified Language.SQL.Keyword as SQL


data Order = Asc | Desc

data Context = Context
               { currentAliasId :: AliasId
               , product :: Maybe QueryProductNode
               , restriction :: Maybe (Expr Bool)
               , orderByRev :: [(Order, String)]
               }

primContext :: Context
primContext =  Context primAlias Nothing Nothing []

nextAliasContext :: Context -> Context
nextAliasContext s = s { currentAliasId =  newAliasId (currentAliasId s) }

updateProduct :: (Maybe QueryProductNode -> QueryProductNode) -> Context -> Context
updateProduct uf ctx =
  ctx { product = Just . uf . product $ ctx }

updateRestriction :: Expr Bool -> Context -> Context
updateRestriction e1 ctx =
  ctx { restriction = Just . uf . restriction $ ctx }
  where uf  Nothing = e1
        uf (Just e0) = e0 `Projectable.and` e1

updateOrderBy :: Order -> Expr t -> Context -> Context
updateOrderBy order e ctx =
  ctx { orderByRev = ((order, showExpr e) :) . orderByRev $ ctx  }

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
          wheres  = Prelude.maybe [] (\e -> [WHERE, SQL.word . showExpr $ e])
          order Asc  = ASC
          order Desc = DESC
          orderList = foldl' (\ r (o, e) -> [SQL.word e, order o] `SQL.sepBy` " "  : r) [] odRev
          orders | null odRev = []
                 | otherwise  = [ORDER, BY, orderList `SQL.sepBy` ", "]

composeSQL :: Projection r -> Context -> String
composeSQL pj c = composedSQL pj
                  (maybe (error "relation: empty product!") (Product.tree . Product.nodeTree) (product c))
                  (restriction c)
                  (orderByRev c)

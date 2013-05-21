{-# LANGUAGE OverloadedStrings #-}

module Database.Relational.Query.Internal.Context (
  Context, Order (..), OrderBys,

  primContext,
  nextAlias,

  updateProduct, takeProduct, restoreLeft,
  updateRestriction,
  updateOrderBy, takeOrderBys, restoreLowOrderBys,

  composeSQL
  ) where

import Prelude hiding (product)
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Monoid ((<>))
import Control.Applicative (pure)

import Database.Relational.Query.Internal.AliasId (primAlias, AliasId, newAliasId, asColumnN)

import Database.Relational.Query.Expr (Expr, showExpr)

import Database.Relational.Query.Internal.Product (QueryProductNode, QueryProduct, queryProductSQL)
import qualified Database.Relational.Query.Internal.Product as Product

import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import qualified Database.Relational.Query.Projectable as Projectable

import Language.SQL.Keyword (Keyword(..), unwordsSQL)
import qualified Language.SQL.Keyword as SQL


data Order = Asc | Desc

type OrderBys = DList (Order, String)

data Context = Context
               { currentAliasId :: AliasId
               , product :: Maybe QueryProductNode
               , restriction :: Maybe (Expr Bool)
               , orderBys :: OrderBys
               }

primContext :: Context
primContext =  Context primAlias Nothing Nothing DList.empty

nextAlias :: Context -> (AliasId, Context)
nextAlias s = (cur, s { currentAliasId =  newAliasId cur })  where
  cur = currentAliasId s

updateProduct' :: (Maybe QueryProductNode -> Maybe QueryProductNode) -> Context -> Context
updateProduct' uf ctx = ctx { product = uf . product $ ctx }

updateProduct :: (Maybe QueryProductNode -> QueryProductNode) -> Context -> Context
updateProduct uf = updateProduct' (Just . uf)

takeProduct :: Context -> (Maybe QueryProductNode, Context)
takeProduct ctx = (product ctx, updateProduct' (const Nothing) ctx)

restoreLeft :: QueryProductNode -> Product.NodeAttr -> Context -> Context
restoreLeft pL naR ctx = updateProduct (Product.growLeft pL naR) ctx

updateRestriction :: Expr Bool -> Context -> Context
updateRestriction e1 ctx =
  ctx { restriction = Just . uf . restriction $ ctx }
  where uf  Nothing = e1
        uf (Just e0) = e0 `Projectable.and` e1

updateOrderBy :: Order -> Expr t -> Context -> Context
updateOrderBy order e ctx =
  ctx { orderBys = orderBys ctx <> pure (order, showExpr e)  }

takeOrderBys :: Context -> (OrderBys, Context)
takeOrderBys ctx = (orderBys ctx , ctx { orderBys = DList.empty })

restoreLowOrderBys :: OrderBys -> Context -> Context
restoreLowOrderBys ros ctx = ctx { orderBys = orderBys ctx <> ros }

composeSQL' :: Projection r -> QueryProduct -> Maybe (Expr Bool) -> OrderBys -> String
composeSQL' pj pd re odRev =
  unwordsSQL
  $ [SELECT, columns' `SQL.sepBy` ", ",
     FROM, SQL.word . queryProductSQL $ pd]
  ++ wheres re
  ++ orders
    where columns' = zipWith
                    (\f n -> SQL.word f `asColumnN` n)
                    (Projection.columns pj)
                    [(0 :: Int)..]
          wheres  = Prelude.maybe [] (\e -> [WHERE, SQL.word . showExpr $ e])
          order Asc  = ASC
          order Desc = DESC
          orderList = DList.foldr (\ (o, e) r -> [SQL.word e, order o] `SQL.sepBy` " "  : r) [] odRev
          orders | null orderList = []
                 | otherwise      = [ORDER, BY, orderList `SQL.sepBy` ", "]

composeSQL :: Projection r -> Context -> String
composeSQL pj c = composeSQL' pj
                  (maybe (error "relation: empty product!") (Product.nodeTree) (product c))
                  (restriction c)
                  (orderBys c)

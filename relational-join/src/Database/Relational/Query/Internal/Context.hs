{-# LANGUAGE OverloadedStrings #-}

module Database.Relational.Query.Internal.Context (
  Context,

  primeContext,
  nextAlias,

  updateProduct, takeProduct, restoreLeft,
  addRestriction,

  composeSQL,

  Order (..), OrderBys,
  OrderingContext,

  primeOrderingContext,

  updateOrderBy, takeOrderBys, restoreLowOrderBys,

  composeOrderBys
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


data Context = Context
               { currentAliasId :: AliasId
               , product :: Maybe QueryProductNode
               , restriction :: Maybe (Expr Bool)
               }

primeContext :: Context
primeContext =  Context primAlias Nothing Nothing

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

addRestriction :: Expr Bool -> Context -> Context
addRestriction e1 ctx =
  ctx { restriction = Just . uf . restriction $ ctx }
  where uf  Nothing = e1
        uf (Just e0) = e0 `Projectable.and` e1

composeSQL' :: Projection r -> QueryProduct -> Maybe (Expr Bool) -> String
composeSQL' pj pd re =
  unwordsSQL
  $ [SELECT, columns' `SQL.sepBy` ", ",
     FROM, SQL.word . queryProductSQL $ pd]
  ++ wheres re
    where columns' = zipWith
                    (\f n -> SQL.word f `asColumnN` n)
                    (Projection.columns pj)
                    [(0 :: Int)..]
          wheres  = Prelude.maybe [] (\e -> [WHERE, SQL.word . showExpr $ e])

composeSQL :: Projection r -> Context -> String
composeSQL pj c = composeSQL' pj
                  (maybe (error "relation: empty product!") (Product.nodeTree) (product c))
                  (restriction c)


data Order = Asc | Desc

type OrderBys = DList (Order, String)

newtype OrderingContext = OrderingContext { orderBys :: OrderBys }

primeOrderingContext :: OrderingContext
primeOrderingContext =  OrderingContext DList.empty

updateOrderBy :: Order -> String -> OrderingContext -> OrderingContext
updateOrderBy order' term ctx =
  ctx { orderBys = orderBys ctx <> pure (order', term)  }

takeOrderBys :: OrderingContext -> (OrderBys, OrderingContext)
takeOrderBys ctx = (orderBys ctx , ctx { orderBys = DList.empty })

restoreLowOrderBys :: OrderBys -> OrderingContext -> OrderingContext
restoreLowOrderBys ros ctx = ctx { orderBys = orderBys ctx <> ros }

order :: Order -> Keyword
order Asc  = ASC
order Desc = DESC

composeOrderBys :: OrderingContext -> String
composeOrderBys oc = unwordsSQL orders  where
  orderList = DList.foldr (\ (o, e) r -> [SQL.word e, order o] `SQL.sepBy` " "  : r) []
              $ orderBys oc
  orders | null orderList = []
         | otherwise      = [ORDER, BY, orderList `SQL.sepBy` ", "]

{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Database.Relational.Query.Internal.Context
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides context definition for
-- "Database.Relational.Query.Monad.Trans.Join" and
-- "Database.Relational.Query.Monad.Trans.Ordering".
module Database.Relational.Query.Internal.Context (
  -- * Join context
  Context,

  primeContext,

  updateProduct, -- takeProduct, restoreLeft,
  addRestriction,

  composeSQL,

  -- * Ordering context
  Order (..), OrderBys,
  OrderingContext,

  primeOrderingContext,

  updateOrderBy, -- takeOrderBys, restoreLowOrderBys,

  composeOrderBys
  ) where

import Prelude hiding (product)
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Monoid ((<>))
import Control.Applicative (pure)

import Database.Relational.Query.Expr (Expr, showExpr, fromTriBool, exprAnd)
import Database.Relational.Query.Sub (asColumnN)

import Database.Relational.Query.Internal.Product (QueryProductNode, QueryProduct, queryProductSQL)
import qualified Database.Relational.Query.Internal.Product as Product

import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection

import Language.SQL.Keyword (Keyword(..), unwordsSQL)
import qualified Language.SQL.Keyword as SQL


-- | Context type for QueryJoin.
data Context = Context
               { product :: Maybe QueryProductNode
               , restriction :: Maybe (Expr Bool)
               }

-- | Initial 'Context'.
primeContext :: Context
primeContext =  Context Nothing Nothing

-- | Update product of 'Context'.
updateProduct' :: (Maybe QueryProductNode -> Maybe QueryProductNode) -> Context -> Context
updateProduct' uf ctx = ctx { product = uf . product $ ctx }

-- | Update product of 'Context'.
updateProduct :: (Maybe QueryProductNode -> QueryProductNode) -> Context -> Context
updateProduct uf = updateProduct' (Just . uf)

-- takeProduct :: Context -> (Maybe QueryProductNode, Context)
-- takeProduct ctx = (product ctx, updateProduct' (const Nothing) ctx)

-- restoreLeft :: QueryProductNode -> Product.NodeAttr -> Context -> Context
-- restoreLeft pL naR ctx = updateProduct (Product.growLeft pL naR) ctx

-- | Add restriction of 'Context'.
addRestriction :: Expr (Maybe Bool) -> Context -> Context
addRestriction e1 ctx =
  ctx { restriction = Just . uf . restriction $ ctx }
  where uf  Nothing  = fromTriBool e1
        uf (Just e0) = e0 `exprAnd` fromTriBool e1

-- | Compose SQL String from QueryJoin monad object.
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

-- | Compose SQL String from QueryJoin monad object.
composeSQL :: Projection r -> Context -> String
composeSQL pj c = composeSQL' pj
                  (maybe (error "relation: empty product!") (Product.nodeTree) (product c))
                  (restriction c)


-- | Order attribute.
data Order = Asc | Desc

-- | Order-by terms.
type OrderBys = DList (Order, String)

-- | Context type for Orderings.
newtype OrderingContext = OrderingContext { orderBys :: OrderBys }

-- | Initial 'OrderingContext'
primeOrderingContext :: OrderingContext
primeOrderingContext =  OrderingContext DList.empty

-- | Add order-by term.
updateOrderBy :: Order -> String -> OrderingContext -> OrderingContext
updateOrderBy order' term ctx =
  ctx { orderBys = orderBys ctx <> pure (order', term)  }

{-
takeOrderBys :: OrderingContext -> (OrderBys, OrderingContext)
takeOrderBys ctx = (orderBys ctx , ctx { orderBys = DList.empty })

restoreLowOrderBys :: OrderBys -> OrderingContext -> OrderingContext
restoreLowOrderBys ros ctx = ctx { orderBys = orderBys ctx <> ros }
-}

-- | Get SQL keyword from order attribute.
order :: Order -> Keyword
order Asc  = ASC
order Desc = DESC

-- | Concatinate order-by terms into SQL string.
composeOrderBys :: OrderingContext -> String
composeOrderBys oc = unwordsSQL orders  where
  orderList = DList.foldr (\ (o, e) r -> [SQL.word e, order o] `SQL.sepBy` " "  : r) []
              $ orderBys oc
  orders | null orderList = []
         | otherwise      = [ORDER, BY, orderList `SQL.sepBy` ", "]

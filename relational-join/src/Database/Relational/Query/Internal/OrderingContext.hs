{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Database.Relational.Query.Internal.OrderingContext
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides context definition for
-- "Database.Relational.Query.Monad.Trans.Ordering".
module Database.Relational.Query.Internal.OrderingContext (
  -- * Ordering context
  Order (..), OrderBys,
  OrderingContext,

  primeOrderingContext,

  updateOrderBy, -- takeOrderBys, restoreLowOrderBys,

  composeOrderBys
  ) where

import Data.DList (DList)
import qualified Data.DList as DList
import Data.Monoid ((<>))
import Control.Applicative (pure)

import Language.SQL.Keyword (Keyword(..), unwordsSQL)
import qualified Language.SQL.Keyword as SQL


-- | Order direction. Ascendant or Descendant.
data Order = Asc | Desc

-- | Ordering terms.
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

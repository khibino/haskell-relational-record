-- |
-- Module      : Database.Relational.Query.Monad.Trans.OrderingState
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides context definition for
-- "Database.Relational.Query.Monad.Trans.Ordering".
module Database.Relational.Query.Monad.Trans.OrderingState (
  -- * Ordering context
  Order, OrderBys,
  OrderingContext,

  primeOrderingContext,

  updateOrderBy, -- takeOrderBys, restoreLowOrderBys,

  orderingTerms
  ) where

import Data.DList (DList)
import qualified Data.DList as DList
import Data.Monoid ((<>))
import Control.Applicative (pure)

import Database.Relational.Query.Sub (Order, OrderColumn, OrderingTerm, OrderingTerms)


-- | Ordering terms.
type OrderBys = DList OrderingTerm

-- | Context type for Orderings.
newtype OrderingContext = OrderingContext { orderBys :: OrderBys }

-- | Initial 'OrderingContext'
primeOrderingContext :: OrderingContext
primeOrderingContext =  OrderingContext DList.empty

-- | Add order-by term.
updateOrderBy :: Order -> OrderColumn -> OrderingContext -> OrderingContext
updateOrderBy order' term ctx =
  ctx { orderBys = orderBys ctx <> pure (order', term)  }

{-
takeOrderBys :: OrderingContext -> (OrderBys, OrderingContext)
takeOrderBys ctx = (orderBys ctx , ctx { orderBys = DList.empty })

restoreLowOrderBys :: OrderBys -> OrderingContext -> OrderingContext
restoreLowOrderBys ros ctx = ctx { orderBys = orderBys ctx <> ros }
-}

-- | Finalize context to extract accumulated ordering state.
orderingTerms :: OrderingContext -> OrderingTerms
orderingTerms =  DList.toList . orderBys

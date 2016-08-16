-- |
-- Module      : Database.Relational.Query.Monad.Trans.JoinState
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides state definition for
-- "Database.Relational.Query.Monad.Trans.Join".
module Database.Relational.Query.Monad.Trans.JoinState (
  -- * Join context
  JoinContext, primeJoinContext, updateProduct, joinProduct
  ) where

import Prelude hiding (product)

import qualified Database.Relational.Query.Sub as Product
import Database.Relational.Query.Sub (ProductBuilder, JoinProduct)


-- | JoinContext type for QueryJoin.
newtype JoinContext =
  JoinContext
  { product  :: Maybe ProductBuilder
  }

-- | Initial 'JoinContext'.
primeJoinContext :: JoinContext
primeJoinContext =  JoinContext Nothing

-- | Update product of 'JoinContext'.
updateProduct :: (Maybe ProductBuilder -> ProductBuilder) -> JoinContext -> JoinContext
updateProduct uf ctx = ctx { product = Just . uf . product $ ctx }

-- |  Finalize context to extract accumulated query product.
joinProduct :: JoinContext -> JoinProduct
joinProduct =  fmap Product.nodeTree . product

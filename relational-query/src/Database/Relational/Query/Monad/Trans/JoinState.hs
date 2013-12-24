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
  JoinContext, primeJoinContext, updateProduct, joinProduct,

  setDuplication, duplication
  ) where

import Prelude hiding (product)

import Database.Relational.Query.Component (Duplication (All))
import qualified Database.Relational.Query.Internal.Product as Product
import Database.Relational.Query.Sub (QueryProductNode, JoinProduct)


-- | JoinContext type for QueryJoin.
data JoinContext =
  JoinContext
  { product  :: Maybe QueryProductNode
  , duplicationAttribute :: Duplication
  }

-- | Initial 'JoinContext'.
primeJoinContext :: JoinContext
primeJoinContext =  JoinContext Nothing All

-- | Update product of 'JoinContext'.
updateProduct' :: (Maybe QueryProductNode -> Maybe QueryProductNode) -> JoinContext -> JoinContext
updateProduct' uf ctx = ctx { product = uf . product $ ctx }

-- | Update product of 'JoinContext'.
updateProduct :: (Maybe QueryProductNode -> QueryProductNode) -> JoinContext -> JoinContext
updateProduct uf = updateProduct' (Just . uf)

-- |  Finalize context to extract accumulated query product.
joinProduct :: JoinContext -> JoinProduct
joinProduct =  fmap Product.nodeTree . product

-- | Set duplication attribute.
setDuplication :: Duplication -> JoinContext -> JoinContext
setDuplication da ctx = ctx { duplicationAttribute = da }

-- | Take duplication attribute.
duplication :: JoinContext -> Duplication
duplication =  duplicationAttribute

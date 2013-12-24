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
  JoinContext,

  primeJoinContext,

  updateProduct, -- takeProduct, restoreLeft,

  joinProduct,

  -- composeFrom

  setDistinct, setAll,

  duplication
  ) where

import Prelude hiding (product)

import Database.Relational.Query.Component (Duplication (Distinct, All))
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

-- takeProduct :: JoinContext -> (Maybe QueryProductNode, JoinContext)
-- takeProduct ctx = (product ctx, updateProduct' (const Nothing) ctx)

-- restoreLeft :: QueryProductNode -> Product.NodeAttr -> JoinContext -> JoinContext
-- restoreLeft pL naR ctx = updateProduct (Product.growLeft pL naR) ctx

-- |  Finalize context to extract accumulated query product.
joinProduct :: JoinContext -> JoinProduct
joinProduct =  fmap Product.nodeTree . product

-- | Set duplication attribute to Distinct.
setDistinct :: JoinContext -> JoinContext
setDistinct ctx = ctx { duplicationAttribute = Distinct }

-- | Set duplication attribute to All.
setAll :: JoinContext -> JoinContext
setAll ctx = ctx { duplicationAttribute = All }

duplication :: JoinContext -> Duplication
duplication =  duplicationAttribute

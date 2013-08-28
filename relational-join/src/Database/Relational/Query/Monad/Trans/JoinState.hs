{-# LANGUAGE OverloadedStrings #-}

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

  composeFrom
  ) where

import Prelude hiding (product)

import qualified Database.Relational.Query.Internal.Product as Product
import Database.Relational.Query.Sub (QueryProductNode, QueryProduct, JoinProduct, queryProductSQL)

import Language.SQL.Keyword (Keyword(..), unwordsSQL)
import qualified Language.SQL.Keyword as SQL


-- | JoinContext type for QueryJoin.
newtype JoinContext =
  JoinContext
  { product :: Maybe QueryProductNode }

-- | Initial 'JoinContext'.
primeJoinContext :: JoinContext
primeJoinContext =  JoinContext Nothing

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

-- | Compose SQL String from 'JoinContext' object.
composeFrom' :: QueryProduct -> String
composeFrom' pd =
  unwordsSQL
  $ [FROM, SQL.word . queryProductSQL $ pd]

-- | Compose SQL String from 'JoinContext' object.
composeFrom :: JoinContext -> String
composeFrom =  composeFrom' . maybe (error "relation: empty product!") ( Product.nodeTree) . product

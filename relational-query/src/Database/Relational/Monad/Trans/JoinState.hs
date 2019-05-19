-- |
-- Module      : Database.Relational.Monad.Trans.JoinState
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides state definition for
-- "Database.Relational.Monad.Trans.Join".
--
-- This is not public interface.
module Database.Relational.Monad.Trans.JoinState (
  -- * Join context
  JoinContext, primeJoinContext, updateProduct, joinProduct
  ) where

import Prelude hiding (product)
import Data.DList (DList, toList)

import Database.Relational.Internal.ContextType (Flat)
import Database.Relational.SqlSyntax
  (JoinProduct, Node, Predicate, untypeRecord)
import qualified Database.Relational.SqlSyntax as Product


-- | JoinContext type for QueryJoin.
newtype JoinContext =
  JoinContext
  { product  :: Maybe (Node (DList (Predicate Flat)))
  }

-- | Initial 'JoinContext'.
primeJoinContext :: JoinContext
primeJoinContext =  JoinContext Nothing

-- | Update product of 'JoinContext'.
updateProduct :: (Maybe (Node (DList (Predicate Flat))) -> Node (DList (Predicate Flat)))
              -> JoinContext
              -> JoinContext
updateProduct uf ctx = ctx { product = Just . uf . product $ ctx }

-- |  Finalize context to extract accumulated query product.
joinProduct :: JoinContext -> JoinProduct
joinProduct =  fmap (fmap (map untypeRecord . toList) . Product.nodeTree) . product

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
  Context,

  primeContext,

  updateProduct, -- takeProduct, restoreLeft,

  composeSQL
  ) where

import Prelude hiding (product)

import Database.Relational.Query.Sub (asColumnN)

import Database.Relational.Query.Internal.Product (QueryProductNode, QueryProduct, queryProductSQL)
import qualified Database.Relational.Query.Internal.Product as Product

import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection

import Language.SQL.Keyword (Keyword(..), unwordsSQL)
import qualified Language.SQL.Keyword as SQL


-- | Context type for QueryJoin.
data Context = Context
               { product :: Maybe QueryProductNode }

-- | Initial 'Context'.
primeContext :: Context
primeContext =  Context Nothing

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

-- | Compose SQL String from QueryJoin monad object.
composeSQL' :: Projection r -> QueryProduct -> String
composeSQL' pj pd =
  unwordsSQL
  $ [SELECT, columns' `SQL.sepBy` ", ",
     FROM, SQL.word . queryProductSQL $ pd]
  where columns' = zipWith
                   (\f n -> SQL.word f `asColumnN` n)
                   (Projection.columns pj)
                   [(0 :: Int)..]

-- | Compose SQL String from QueryJoin monad object.
composeSQL :: Projection r -> Context -> String
composeSQL pj c = composeSQL' pj
                  (maybe (error "relation: empty product!") (Product.nodeTree) (product c))


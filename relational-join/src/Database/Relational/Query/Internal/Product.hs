-- |
-- Module      : Database.Relational.Query.Internal.Product
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines product structure to compose SQL join.
module Database.Relational.Query.Internal.Product (
  -- * Product tree type
  NodeAttr (..), ProductTree (..),
  Node, node, nodeAttr, nodeTree,
  growRight, -- growLeft,
  growProduct, product, restrictProduct,
  ) where

import Prelude hiding (and, product)
import Database.Relational.Query.Context (Flat)
import Database.Relational.Query.Expr (exprAnd)
import qualified Database.Relational.Query.Expr as Expr
import Data.Monoid ((<>))
import Data.Foldable (Foldable (foldMap))


type Expr = Expr.Expr Flat

-- | node attribute for product.
data NodeAttr = Just' | Maybe

-- | Product tree type. Product tree is constructed by left node and right node.
data ProductTree q = Leaf q
                   | Join !(Node q) !(Node q) !(Maybe (Expr Bool))

-- | Product node. node attribute and product tree.
data Node q = Node !NodeAttr !(ProductTree q)

-- | Get node attribute.
nodeAttr :: Node q -> NodeAttr
nodeAttr (Node a _) = a  where

-- | Get tree from node.
nodeTree :: Node q -> ProductTree q
nodeTree (Node _ t) = t

-- | Foldable instance of ProductTree
instance Foldable ProductTree where
  foldMap f pq = rec pq where
    rec (Leaf q) = f q
    rec (Join (Node _ lp) (Node _ rp) _ ) = rec lp <> rec rp

-- | Make product node from node attribute and product tree.
node :: NodeAttr      -- ^ Node attribute
     -> ProductTree q -- ^ Product tree
     -> Node q        -- ^ Result node
node =  Node

-- | Push new tree into product right term.
growRight :: Maybe (Node q)            -- ^ Current tree
          -> (NodeAttr, ProductTree q) -- ^ New tree to push into right
          -> Node q                    -- ^ Result node
growRight = d  where
  d Nothing  (naR, q) = node naR q
  d (Just l) (naR, q) = node Just' $ Join l (node naR q) Nothing

-- -- | Push new tree node into product left term.
-- growLeft :: Node q         -- ^ New node to push into left
--          -> NodeAttr       -- ^ Node attribute to replace rigth node attribute.
--          -> Maybe (Node q) -- ^ Current tree
--          -> Node q         -- ^ Result node
-- growLeft =  d  where
--   d q _naR Nothing  = q -- error is better?
--   d q naR  (Just r) = node Just' $ Join q (node naR (nodeTree r)) Nothing

-- | Push new leaf node into product right term.
growProduct :: Maybe (Node q) -- ^ Current tree
            -> (NodeAttr, q)  -- ^ New leaf to push into right
            -> Node q         -- ^ Result node
growProduct =  match  where
  match t (na, q) =  growRight t (na, Leaf q)

-- | Just make product of two node.
product :: Node q            -- ^ Left node
        -> Node q            -- ^ Right node
        -> Maybe (Expr Bool) -- ^ Join restriction
        -> ProductTree q     -- ^ Result tree
product =  Join

-- | Add restriction into top product of product tree.
restrictProduct' :: ProductTree q -- ^ Product to restrict
                 -> Expr Bool     -- ^ Restriction to add
                 -> ProductTree q -- ^ Result product
restrictProduct' =  d  where
  d (Join lp rp Nothing)   rs' = Join lp rp (Just rs')
  d (Join lp rp (Just rs)) rs' = Join lp rp (Just $ rs `exprAnd` rs')
  d leaf'@(Leaf _)         _   = leaf' -- or error on compile

-- | Add restriction into top product of product tree node.
restrictProduct :: Node q    -- ^ Target node which has product to restrict
                -> Expr Bool -- ^ Restriction to add
                -> Node q    -- ^ Result node
restrictProduct (Node a t) e = node a (restrictProduct' t e)

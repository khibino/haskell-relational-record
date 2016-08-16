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
  -- * Interfaces to manipulate ProductTree type
  growProduct, restrictProduct,
  ) where

import Prelude hiding (and, product)
import Control.Applicative (pure, empty)
import Data.Monoid ((<>))

import Database.Relational.Query.Context (Flat)
import Database.Relational.Query.Internal.Sub (NodeAttr (..), ProductTree (..), Node (..), Projection)


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
  d (Just l) (naR, q) = node Just' $ Join l (node naR q) empty

-- | Push new leaf node into product right term.
growProduct :: Maybe (Node q) -- ^ Current tree
            -> (NodeAttr, q)  -- ^ New leaf to push into right
            -> Node q         -- ^ Result node
growProduct =  match  where
  match t (na, q) =  growRight t (na, Leaf q)

-- | Add restriction into top product of product tree.
restrictProduct' :: ProductTree q                -- ^ Product to restrict
                 -> Projection Flat (Maybe Bool) -- ^ Restriction to add
                 -> ProductTree q                -- ^ Result product
restrictProduct' =  d  where
  d (Join lp rp rs) rs' = Join lp rp (rs <> pure rs')
  d leaf'@(Leaf _)         _   = leaf' -- or error on compile

-- | Add restriction into top product of product tree node.
restrictProduct :: Node q                       -- ^ Target node which has product to restrict
                -> Projection Flat (Maybe Bool) -- ^ Restriction to add
                -> Node q                       -- ^ Result node
restrictProduct (Node a t) e = node a (restrictProduct' t e)

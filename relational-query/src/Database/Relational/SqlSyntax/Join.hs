-- |
-- Module      : Database.Relational.SqlSyntax.Join
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines product structure to compose SQL join.
module Database.Relational.SqlSyntax.Join (
  -- * Interfaces to manipulate ProductTree type
  growProduct, restrictProduct,
  ) where

import Prelude hiding (and, product)
import Control.Applicative (pure)
import Data.Monoid ((<>), mempty)

import Database.Relational.Internal.ContextType (Flat)
import Database.Relational.SqlSyntax.Types
  (NodeAttr (..), ProductTree (..), Node (..), Record, Qualified, SubQuery,
   QueryRestrictionBuilder)


-- | Push new tree into product right term.
growRight :: Maybe (Node QueryRestrictionBuilder)            -- ^ Current tree
          -> (NodeAttr, ProductTree QueryRestrictionBuilder) -- ^ New tree to push into right
          -> Node QueryRestrictionBuilder                    -- ^ Result node
growRight = d  where
  d Nothing  (naR, q) = Node naR q
  d (Just l) (naR, q) = Node Just' $ Join l (Node naR q) mempty

-- | Push new leaf node into product right term.
growProduct :: Maybe (Node QueryRestrictionBuilder) -- ^ Current tree
            -> (NodeAttr, Qualified SubQuery)       -- ^ New leaf to push into right
            -> Node QueryRestrictionBuilder         -- ^ Result node
growProduct =  match  where
  match t (na, q) =  growRight t (na, Leaf q)

-- | Add restriction into top product of product tree.
restrictProduct' :: ProductTree QueryRestrictionBuilder -- ^ Product to restrict
                 -> Record Flat (Maybe Bool)            -- ^ Restriction to add
                 -> ProductTree QueryRestrictionBuilder -- ^ Result product
restrictProduct' =  d  where
  d (Join lp rp rs) rs' = Join lp rp (rs <> pure rs')
  d leaf'@(Leaf _)         _   = leaf' -- or error on compile

-- | Add restriction into top product of product tree node.
restrictProduct :: Node QueryRestrictionBuilder -- ^ Target node which has product to restrict
                -> Record Flat (Maybe Bool)     -- ^ Restriction to add
                -> Node QueryRestrictionBuilder -- ^ Result node
restrictProduct (Node a t) e = Node a (restrictProduct' t e)

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
  NodeAttr (Just', Maybe),
  ProductTree, Node, QueryProduct, QueryProductNode,
  nodeTree, growRight, growLeft,
  growProduct, product, restrictProduct,

  -- * Compose joined SQL
  queryProductSQL
  ) where

import Prelude hiding (and, product)
import Database.Relational.Query.Expr (Expr, showExpr, fromTriBool, exprAnd)
import Database.Relational.Query.Projectable (valueTrue)
import Database.Relational.Query.Sub (SubQuery, Qualified)
import qualified Database.Relational.Query.Sub as SubQuery

import Database.Relational.Query.Internal.ShowS
  (showUnwordsSQL, showWordSQL, showUnwords)
import Language.SQL.Keyword (Keyword(..))

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Foldable (Foldable (foldMap))


-- | Node type for product.
data NodeAttr = Just' | Maybe

-- | Product tree type. Product tree is constructed by left node and right node.
data ProductTree q = Leaf q
                   | Join !(Node q) !(Node q) !(Maybe (Expr Bool))

-- | Product node. Node type and product tree.
data Node q = Node !NodeAttr !(ProductTree q)

-- | Get node type.
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

-- | Product tree specialized by 'SubQuery'.
type QueryProduct = ProductTree (Qualified SubQuery)
-- | Product node specialized by 'SubQuery'.
type QueryProductNode = Node (Qualified SubQuery)

-- | Make product node from node type and product tree.
node :: NodeAttr -> ProductTree q -> Node q
node =  Node

-- | Push new tree into product right term.
growRight :: Maybe (Node q) -> (NodeAttr, ProductTree q) -> Node q
growRight = d  where
  d Nothing  (naR, q) = node naR q
  d (Just l) (naR, q) = node Just' $ Join l (node naR q) Nothing

-- | Push new tree node into product left term.
growLeft :: Node q -> NodeAttr -> Maybe (Node q) -> Node q
growLeft =  d  where
  d q _naR Nothing  = q -- error is better?
  d q naR  (Just r) = node Just' $ Join q (node naR (nodeTree r)) Nothing

-- | Push new leaf node into product left term.
growProduct :: Maybe (Node q) -> (NodeAttr, q) -> Node q
growProduct =  match  where
  match t (na, q) =  growRight t (na, Leaf q)

-- | Just make product of two node.
product :: Node q -> Node q -> Maybe (Expr Bool) -> ProductTree q
product =  Join

-- | Add restriction into top product of product tree.
restrictProduct' :: ProductTree q -> Expr Bool -> ProductTree q
restrictProduct' =  d  where
  d (Join lp rp Nothing)   rs' = Join lp rp (Just rs')
  d (Join lp rp (Just rs)) rs' = Join lp rp (Just $ rs `exprAnd` rs')
  d leaf'@(Leaf _)         _   = leaf' -- or error on compile

-- | Add restriction into top product of product tree node.
restrictProduct :: Node q -> Expr Bool -> Node q
restrictProduct (Node a t) e = node a (restrictProduct' t e)


-- | Show product tree of query into SQL. ShowS result.
showQueryProduct :: QueryProduct -> ShowS
showQueryProduct =  rec  where
  joinType Just' Just' = INNER
  joinType Just' Maybe = LEFT
  joinType Maybe Just' = RIGHT
  joinType Maybe Maybe = FULL
  urec (Node _ p@(Leaf _))     = rec p
  urec (Node _ p@(Join _ _ _)) = showParen True (rec p)
  rec (Leaf q)               = showString $ SubQuery.qualifiedForm q
  rec (Join left' right' rs) =
    showUnwords
    [urec left',
     showUnwordsSQL [joinType (nodeAttr left') (nodeAttr right'), JOIN],
     urec right',
     showWordSQL ON,
     showString . showExpr
     . fromMaybe (fromTriBool valueTrue) {- or error on compile -}  $ rs]

-- | Show product tree of query into SQL.
queryProductSQL :: QueryProduct -> String
queryProductSQL =  ($ "") . showQueryProduct

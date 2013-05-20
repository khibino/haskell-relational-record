module Database.Relational.Query.Product (
  NodeAttr (Just', Maybe),
  ProductTree, Node, QueryProduct, QueryProductNode,
  growRight, growLeft,
  growProduct, product, restrictProduct,
  Product,
  tree,
  productSQL
  ) where

import Prelude hiding (and, product)
import Database.Relational.Query.Expr (Expr, showExpr)
import Database.Relational.Query.Projectable (valueTrue, and)
import Database.Relational.Query.AliasId (Qualified)
import Database.Relational.Query.Sub (SubQuery)
import qualified Database.Relational.Query.Sub as SubQuery

import Language.SQL.Keyword (Keyword(..), unwordsSQL)
import qualified Language.SQL.Keyword as SQL

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Foldable (Foldable (foldMap))


data NodeAttr = Just' | Maybe

data ProductTree q = Leaf q
                   | Join !(Node q) !(Node q) !(Maybe (Expr Bool))

data Node q = Node !NodeAttr !(ProductTree q)

nodeAttr :: Node q -> NodeAttr
nodeAttr (Node a _) = a  where

nodeTree :: Node q -> ProductTree q
nodeTree (Node _ t) = t

instance Foldable ProductTree where
  foldMap f pq = rec pq where
    rec (Leaf q) = f q
    rec (Join (Node _ lp) (Node _ rp) _ ) = rec lp <> rec rp

type QueryProduct = ProductTree (Qualified SubQuery)
type QueryProductNode = Node (Qualified SubQuery)

node :: NodeAttr -> ProductTree q -> Node q
node =  Node

growRight :: Maybe (Node q) -> (NodeAttr, ProductTree q) -> Node q
growRight = d  where
  d Nothing  (naR, q) = node naR q
  d (Just l) (naR, q) = node Just' $ Join l (node naR q) Nothing

growLeft :: Node q -> NodeAttr -> Maybe (Node q) -> Node q
growLeft =  d  where
  d q _naR Nothing  = q -- error is better?
  d q naR  (Just r) = node Just' $ Join q (node naR (nodeTree r)) Nothing

growProduct :: Maybe (Node q) -> (NodeAttr, q) -> Node q
growProduct =  match  where
  match t (na, q) =  growRight t (na, Leaf q)


product :: Node q -> Node q -> Maybe (Expr Bool) -> ProductTree q
product =  Join

restrictProduct' :: ProductTree q -> Expr Bool -> ProductTree q
restrictProduct' =  d  where
  d (Join lp rp Nothing)   rs' = Join lp rp (Just rs')
  d (Join lp rp (Just rs)) rs' = Join lp rp (Just $ rs `and` rs')
  d leaf'@(Leaf _)         _   = leaf' -- or error on compile

restrictProduct :: Node q -> Expr Bool -> Node q
restrictProduct (Node a t) e = node a (restrictProduct' t e)


newtype Product = Tree QueryProductNode

tree :: QueryProductNode -> Product
tree =  Tree

showParen' :: ShowS -> ShowS
showParen' =  showParen True

showWordsSQL :: [SQL.Keyword] -> ShowS
showWordsSQL =  showString . unwordsSQL

showWordSQL :: SQL.Keyword -> ShowS
showWordSQL =  showString . SQL.wordShow

showSpace :: ShowS
showSpace =  showChar ' '

showUnwords :: [ShowS] -> ShowS
showUnwords =  rec  where
  rec []     = showString ""
  rec (s:ss) = s . showSpace . rec ss

showQueryProduct :: QueryProduct -> ShowS
showQueryProduct =  rec  where
  joinType Just' Just' = INNER
  joinType Just' Maybe = LEFT
  joinType Maybe Just' = RIGHT
  joinType Maybe Maybe = FULL
  urec (Node _ p@(Leaf _))     = rec p
  urec (Node _ p@(Join _ _ _)) = showParen' (rec p)
  rec (Leaf q)               = showString $ SubQuery.qualifiedForm q
  rec (Join left' right' rs) =
    showUnwords
    [urec left',
     showWordsSQL [joinType (nodeAttr left') (nodeAttr right'), JOIN],
     urec right',
     showWordSQL ON,
     showString . showExpr . fromMaybe valueTrue {- or error on compile -}  $ rs]

productSQL :: Product -> String
productSQL =  d  where
  d (Tree (Node _ pt))     = ($ "") . showQueryProduct $ pt

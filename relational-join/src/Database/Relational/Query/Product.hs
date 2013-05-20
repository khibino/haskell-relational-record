module Database.Relational.Query.Product (
  NodeAttr (Just', Maybe), -- unsafeUpdateNodeAttr,
  ProductTree, Node, QueryProduct, QueryProductNode,
  node, growRight, growLeft,
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

data ProductTree q = Leaf NodeAttr q
                   | Join NodeAttr !(Node q) !(Node q) !(Maybe (Expr Bool))

type Node q = ProductTree q

nodeAttr :: Node q -> NodeAttr
nodeAttr =  d  where
  d (Leaf jt _)     = jt
  d (Join jt _ _ _) = jt

unsafeUpdateNodeAttr :: NodeAttr -> ProductTree q -> ProductTree q
unsafeUpdateNodeAttr a = d  where
  d (Leaf _ q)     = Leaf a q
  d (Join _ l r c) = Join a l r c


instance Foldable ProductTree where
  foldMap f pq = rec pq where
    rec (Leaf _ q) = f q
    rec (Join _ lp rp _ ) = rec lp <> rec rp

type QueryProduct = ProductTree (Qualified SubQuery)
type QueryProductNode = Node (Qualified SubQuery)

node :: NodeAttr -> ProductTree q -> Node q
node a q = unsafeUpdateNodeAttr a q

leaf :: NodeAttr -> q -> Node q
leaf a q = node a (Leaf a q)

growRight :: Maybe (ProductTree q) -> (NodeAttr, ProductTree q) -> ProductTree q
growRight = d  where
  d Nothing  (na, q) = node na q
  d (Just l) (na, q) = Join Just' l (node na q) Nothing

growLeft :: (NodeAttr, ProductTree q) -> Maybe (ProductTree q) -> ProductTree q
growLeft =  d  where
  d (na, q) Nothing  = node na q
  d (na, q) (Just r) = Join Just' (node na q) r Nothing

growProduct :: Maybe (ProductTree q) -> (NodeAttr, q) -> ProductTree q
growProduct =  match  where
  match t (na, q) =  growRight t (na, leaf na q)


product :: NodeAttr -> ProductTree q -> ProductTree q -> Maybe (Expr Bool) -> ProductTree q
product =  Join

restrictProduct :: ProductTree q -> Expr Bool -> ProductTree q
restrictProduct =  d  where
  d (Join ja lp rp Nothing)   rs' = Join ja lp rp (Just rs')
  d (Join ja lp rp (Just rs)) rs' = Join ja lp rp (Just $ rs `and` rs')
  d leaf'@(Leaf _ _)          _   = leaf' -- or error on compile


newtype Product = Tree QueryProduct

tree :: QueryProduct -> Product
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
  urec p@(Leaf _ _)     = rec p
  urec p@(Join _ _ _ _) = showParen' (rec p)
  rec (Leaf _ q)               = showString $ SubQuery.qualifiedForm q
  rec (Join _ left' right' rs) =
    showUnwords
    [urec left',
     showWordsSQL [joinType (nodeAttr left') (nodeAttr right'), JOIN],
     urec right',
     showWordSQL ON,
     showString . showExpr . fromMaybe valueTrue {- or error on compile -}  $ rs]

productSQL :: Product -> String
productSQL =  d  where
  d (Tree pt)     = ($ "") . showQueryProduct $ pt

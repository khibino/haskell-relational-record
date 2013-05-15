module Database.Relational.Query.Product (
  QueryProduct, ProductTree, growProduct, restrictProduct,
  JoinAttr (Inner, Outer),
  Product,
  tree,
  productSQL
  ) where

import Prelude hiding (and)
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


data JoinAttr = Inner | Outer

data ProductTree q = Leaf JoinAttr q
                   | Join JoinAttr !(ProductTree q) !(ProductTree q) !(Maybe (Expr Bool))

joinAttr :: ProductTree q -> JoinAttr
joinAttr =  d  where
  d (Leaf jt _)     = jt
  d (Join jt _ _ _) = jt


instance Foldable ProductTree where
  foldMap f pq = rec pq where
    rec (Leaf _ q) = f q
    rec (Join _ lp rp _ ) = rec lp <> rec rp

type QueryProduct = ProductTree (Qualified SubQuery)

growProduct :: Maybe (ProductTree q) -> (JoinAttr, q) -> ProductTree q
growProduct =  d  where
  d Nothing  (ja, q) = Leaf ja q
  d (Just t) (ja, q) = Join Inner t (Leaf ja q) Nothing

restrictProduct :: ProductTree q -> Expr Bool -> ProductTree q
restrictProduct =  d  where
  d (Join ja lp rp Nothing)   rs' = Join ja lp rp (Just rs')
  d (Join ja lp rp (Just rs)) rs' = Join ja lp rp (Just $ rs `and` rs')
  d leaf@(Leaf _ _)           _   = leaf -- or error on compile


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
  joinType Inner Inner = INNER
  joinType Inner Outer = LEFT
  joinType Outer Inner = RIGHT
  joinType Outer Outer = FULL
  urec p@(Leaf _ _)     = rec p
  urec p@(Join _ _ _ _) = showParen' (rec p)
  rec (Leaf _ q)               = showWordSQL $ SubQuery.qualifiedForm q
  rec (Join _ left' right' rs) =
    showUnwords
    [urec left',
     showWordsSQL [joinType (joinAttr left') (joinAttr right'), JOIN],
     urec right',
     showWordSQL ON,
     showString . showExpr . fromMaybe valueTrue {- or error on compile -}  $ rs]

productSQL :: Product -> String
productSQL =  d  where
  d (Tree pt)     = ($ "") . showQueryProduct $ pt

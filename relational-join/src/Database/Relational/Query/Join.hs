
module Database.Relational.Query.Join (
  QueryJoin,

  on, wheres, asc, desc,
  table,

  record, expr, compose, (>*<), (!), (!?), flatten, relation,

  inner, outer, from

  -- runQuery
  ) where

import Prelude hiding (product)
import Control.Monad (liftM, ap)
import Control.Applicative (Applicative (pure, (<*>)))

import Database.Record (PersistableWidth)

import Database.Relational.Query.AliasId.Unsafe (primAlias)
import Database.Relational.Query.AliasId (AliasId, newAliasId, Qualified)
import qualified Database.Relational.Query.AliasId as AliasId

import Database.Relational.Query.Table (Table)
-- import Database.Relational.Query.Sub (SubQuery)

import Database.Relational.Query.Expr (Expr, showExpr)
import qualified Database.Relational.Query.Expr as Expr

import Database.Relational.Query.Product
  (QueryProduct, JoinAttr(Inner, Outer), growProduct, restrictProduct)
import qualified Database.Relational.Query.Product as Product

import Database.Relational.Query.Projection (Projection, Projectable(project))
import qualified Database.Relational.Query.Projection as Projection

import Database.Relational.Query.Pi (Pi)

import Database.Relational.Query.Relation (Relation, PrimeRelation, finalizeRelation, Order(Asc, Desc))
import qualified Database.Relational.Query.Relation as Relation

data Context = Context
               { currentAliasId :: AliasId
               , product :: Maybe QueryProduct
               , restriction :: Maybe (Expr Bool)
               , orderByRev :: [(Order, String)]
               }

primContext :: Context
primContext =  Context primAlias Nothing Nothing []

nextAliasContext :: Context -> Context
nextAliasContext s = s { currentAliasId =  newAliasId (currentAliasId s) }

updateProduct' :: (Maybe QueryProduct -> QueryProduct) -> Context -> Context
updateProduct' uf ctx =
  ctx { product = Just . uf . product $ ctx }

updateRestriction' :: Expr Bool -> Context -> Context
updateRestriction' e1 ctx =
  ctx { restriction = Just . uf . restriction $ ctx }
  where uf  Nothing = e1
        uf (Just e0) = e0 `Expr.and` e1

updateOrderBy' :: Order -> Expr t -> Context -> Context
updateOrderBy' order e ctx =
  ctx { orderByRev = ((order, showExpr e) :) . orderByRev $ ctx  }


newtype QueryJoin a =
  QueryJoin { runQueryJoin :: Context -> (a, Context) }

runQueryPrime :: QueryJoin a -> (a, Context)
runQueryPrime q = runQueryJoin q $ primContext

newAlias :: QueryJoin AliasId
newAlias =  QueryJoin
            $ \st -> let st' = nextAliasContext st
                     in  (currentAliasId st, st')

updateContext :: (Context -> Context) -> QueryJoin ()
updateContext uf =
  QueryJoin $ \st -> ((), uf st)

updateProduct :: JoinAttr -> Qualified (Relation r) -> QueryJoin ()
updateProduct attr qrel = updateContext (updateProduct' (`growProduct` (attr, fmap Relation.toSubQuery qrel)))

updateJoinRestriction :: Expr Bool -> QueryJoin ()
updateJoinRestriction e = updateContext (updateProduct' d)  where
  d  Nothing  = error "addProductRestriction: product is empty!"
  d (Just pt) = restrictProduct pt e

updateRestriction :: Expr Bool -> QueryJoin ()
updateRestriction e = updateContext (updateRestriction' e)

updateOrderBy :: Order -> Expr t -> QueryJoin ()
updateOrderBy order e = updateContext (updateOrderBy' order e)


on :: Expr Bool -> QueryJoin ()
on =  updateJoinRestriction

wheres :: Expr Bool -> QueryJoin ()
wheres =  updateRestriction

asc  :: Expr t -> QueryJoin ()
asc  =  updateOrderBy Asc

desc :: Expr t -> QueryJoin ()
desc =  updateOrderBy Desc


table :: Table r -> Relation r
table =  Relation.fromTable

record :: Qualified (Relation r) -> Projection r
record =  Projection.fromQualifiedSubQuery  . fmap Relation.toSubQuery

expr :: Projection ft -> Expr ft
expr =  Projection.toExpr

compose :: Projection a -> Projection b -> Projection (c a b)
compose =  Projection.compose

(>*<) :: Projection a -> Projection b -> Projection (a, b)
(>*<) =  compose

(!) :: (PersistableWidth b, Projectable p) => Projection a -> Pi a b -> p b
p ! pi' = project $ Projection.pi p pi'

(!?) :: (PersistableWidth b, Projectable p) => Projection (Maybe a) -> Pi a b -> p (Maybe b)
p !? pi' = project $ Projection.piMaybe p pi'

flatten :: Projection (Maybe (Maybe a)) -> Projection (Maybe a)
flatten =  Projection.flattenMaybe

infixl 8 !, !?
infixl 1 >*<


instance Monad QueryJoin where
  return rel  = QueryJoin $ \st  -> (rel, st)
  q0 >>= f    = QueryJoin
                $ \st0 -> let (rel0, st1) = runQueryJoin q0       st0
                          in                runQueryJoin (f rel0) st1

instance Functor QueryJoin where
  fmap = liftM

instance Applicative QueryJoin where
  pure  = return
  (<*>) = ap


qualify :: rel -> QueryJoin (Qualified rel)
qualify rel =
  do n <- newAlias
     return $ AliasId.qualify rel n

query :: JoinAttr -> Relation r -> QueryJoin (Qualified (Relation r))
query attr rel =
  do qrel <- qualify rel
     updateProduct attr qrel
     return qrel

inner :: Relation r -> QueryJoin (Projection r)
inner =  fmap record . query Inner

outer :: Relation r -> QueryJoin (Projection (Maybe r))
outer =  fmap (record . fmap Relation.outer) . query Outer

from :: Table r -> QueryJoin (Projection r)
from =  inner . table

relation :: QueryJoin (Projection r) -> PrimeRelation a r
relation q = finalizeRelation projection product' (restriction st) (orderByRev st)  where
  (projection, st) = runQueryPrime q
  product' = maybe (error "relation: empty product!") Product.tree $ product st

-- runQuery :: QueryJoin (Relation r) -> SubQuery
-- runQuery =  Relation.toSubQuery . fst . runQueryPrime

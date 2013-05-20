
module Database.Relational.Query.Join (
  QueryJoin, PlaceHolders,

  on, wheres, asc, desc,
  table,

  record, record', expr, compose, (>*<), (!), (!?), flatten,
  relation, relation',

  query, query', queryMaybe, queryMaybe', from,

  queryMerge, queryMergeMaybe
  ) where

import Prelude hiding (product)
import Control.Monad (liftM, ap)
import Control.Applicative (Applicative (pure, (<*>)))

import Database.Record (PersistableWidth)

import Database.Relational.Query.Internal.Context
  (Context, primContext, currentAliasId, product, restriction, orderByRev,
   nextAliasContext, updateProduct', updateRestriction', updateOrderBy')

import Database.Relational.Query.AliasId (AliasId, Qualified)
import qualified Database.Relational.Query.AliasId as AliasId

import Database.Relational.Query.Table (Table)

import Database.Relational.Query.Expr (Expr)

import Database.Relational.Query.Product
  (NodeAttr(Just', Maybe), growProduct, restrictProduct)
import qualified Database.Relational.Query.Product as Product

import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Projectable (Projectable(project))

import Database.Relational.Query.Pi (Pi)

import Database.Relational.Query.Relation (Relation, PrimeRelation, finalizeRelation, Order(Asc, Desc))
import qualified Database.Relational.Query.Relation as Relation


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

updateProduct :: NodeAttr -> Qualified (PrimeRelation p r) -> QueryJoin ()
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


data PlaceHolders p = PlaceHolders

table :: Table r -> Relation r
table =  Relation.fromTable

record' :: Qualified (PrimeRelation p r) -> (PlaceHolders p, Projection r)
record' qrel =
  (PlaceHolders,
   Projection.fromQualifiedSubQuery (fmap Relation.toSubQuery qrel))

record :: Qualified (Relation r) -> Projection r
record =  snd . record'

expr :: Projection ft -> Expr ft
expr =  project

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

queryWithAttr :: NodeAttr -> PrimeRelation p r -> QueryJoin (Qualified (PrimeRelation p r))
queryWithAttr attr rel =
  do qrel <- qualify rel
     updateProduct attr qrel
     return qrel

query :: Relation r -> QueryJoin (Projection r)
query =  fmap record . queryWithAttr Just'

query' :: PrimeRelation p r -> QueryJoin (PlaceHolders p, Projection r)
query' =  fmap record' . queryWithAttr Just'

queryMaybe :: Relation r -> QueryJoin (Projection (Maybe r))
queryMaybe =  fmap (record . fmap Relation.toMaybe) . queryWithAttr Maybe

queryMaybe' :: PrimeRelation p r -> QueryJoin (PlaceHolders p, Projection (Maybe r))
queryMaybe' =  fmap (record' . fmap Relation.toMaybe) . queryWithAttr Maybe

from :: Table r -> QueryJoin (Projection r)
from =  query . table

unsafeMergeAnother :: NodeAttr -> QueryJoin a -> QueryJoin a
unsafeMergeAnother attr q1 =
  QueryJoin
  $ \st0 -> let mp0       = product st0
                (pj, st1) = runQueryJoin q1 (st0 { product = Nothing})
            in  (pj, maybe st1 (\p0 -> updateProduct' (Product.growLeft p0 attr) st1) mp0)

queryMergeWithAttr :: NodeAttr -> QueryJoin (Projection r) -> QueryJoin (Projection r)
queryMergeWithAttr =  unsafeMergeAnother

queryMerge :: QueryJoin (Projection r) -> QueryJoin (Projection r)
queryMerge =  queryMergeWithAttr Just'

queryMergeMaybe :: QueryJoin (Projection a) -> QueryJoin (Projection (Maybe a))
queryMergeMaybe =  fmap Projection.just . queryMergeWithAttr Maybe

relation :: QueryJoin (Projection r) -> PrimeRelation a r
relation q = finalizeRelation projection product' (restriction st) (orderByRev st)  where
  (projection, st) = runQueryPrime q
  product' = maybe (error "relation: empty product!") (Product.tree . Product.nodeTree) $ product st

relation' :: QueryJoin (PlaceHolders p, Projection r) -> PrimeRelation p r
relation' =  relation . fmap snd

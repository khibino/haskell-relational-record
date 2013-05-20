
module Database.Relational.Query.Join (
  QueryJoin, PlaceHolders,

  on, wheres, asc, desc,
  table,

  expr,
  compose, (>*<), (!), (!?), flatten,
  relation, relation',

  query, query', queryMaybe, queryMaybe', from,

  PrimeRelation, Relation,

  toSQL,

  toSubQuery,

  nested, width
  ) where

import Prelude hiding (product)
import Control.Monad (liftM, ap)
import Control.Applicative (Applicative (pure, (<*>)))

import Database.Record (PersistableWidth)

import Database.Relational.Query.Internal.Context
  (Context, Order(Asc, Desc), primContext, currentAliasId, product, orderByRev,
   nextAliasContext, updateProduct', updateRestriction', updateOrderBy', composeSQL)

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

import Database.Relational.Query.Sub (SubQuery)
import qualified Database.Relational.Query.Sub as SubQuery


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


data PrimeRelation p r = SubQuery SubQuery
                       | PrimeRelation (QueryJoin (Projection r))

type Relation r = PrimeRelation () r

data PlaceHolders p = PlaceHolders

table :: Table r -> Relation r
table =  SubQuery . SubQuery.fromTable

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

unsafeMergeAnother :: NodeAttr -> QueryJoin a -> QueryJoin a
unsafeMergeAnother attr q1 =
  QueryJoin
  $ \st0 -> let mp0       = product st0
                or0       = orderByRev st0
                (pj, st1) = runQueryJoin q1 (st0 { product = Nothing, orderByRev = [] })
            in  (pj,
                 (maybe st1 (\p0 ->
                              updateProduct' (Product.growLeft p0 attr)
                              st1
                            ) mp0) { orderByRev = or0 ++ orderByRev st1 }
                )

queryMergeWithAttr :: NodeAttr -> QueryJoin (Projection r) -> QueryJoin (Projection r)
queryMergeWithAttr =  unsafeMergeAnother

queryWithAttr :: NodeAttr -> PrimeRelation p r -> QueryJoin (PlaceHolders p, Projection r)
queryWithAttr attr = fmap ((,) PlaceHolders) . d where
  d (SubQuery sub)    = do
    qsub <- qualify sub
    updateContext (updateProduct' (`growProduct` (attr, qsub)))
    return $ Projection.fromQualifiedSubQuery qsub
  d (PrimeRelation q) =
    queryMergeWithAttr attr q

query' :: PrimeRelation p r -> QueryJoin (PlaceHolders p, Projection r)
query' =  queryWithAttr Just'

query :: PrimeRelation p r -> QueryJoin (Projection r)
query =  fmap snd . query'

queryMaybe' :: PrimeRelation p r -> QueryJoin (PlaceHolders p, Projection (Maybe r))
queryMaybe' pr =  do
  (ph, pj) <- queryWithAttr Maybe pr
  return (ph, Projection.just pj)

queryMaybe :: PrimeRelation p r -> QueryJoin (Projection (Maybe r))
queryMaybe =  fmap snd . queryMaybe'

relation :: QueryJoin (Projection r) -> PrimeRelation p r
relation =  PrimeRelation

relation' :: QueryJoin (PlaceHolders p, Projection r) -> PrimeRelation p r
relation' =  PrimeRelation . fmap snd

from :: Table r -> QueryJoin (Projection r)
from =  query . table

toSQL :: PrimeRelation p r -> String
toSQL =  d  where
  d (SubQuery sub)     = SubQuery.toSQL sub
  d (PrimeRelation qp) = uncurry composeSQL (runQueryPrime qp)

instance Show (PrimeRelation p r) where
  show = toSQL

toSubQuery :: PrimeRelation p r -> SubQuery
toSubQuery =  d  where
  d (SubQuery sub)     = sub
  d (PrimeRelation qp) = SubQuery.subQuery (composeSQL pj c) (Projection.width pj) where
    (pj, c) = runQueryPrime qp

width :: PrimeRelation p r -> Int
width =  SubQuery.width . toSubQuery

nested :: PrimeRelation p r -> PrimeRelation p r
nested =  SubQuery . toSubQuery


module Database.Relational.Query.Join (
  QueryJoin,

  on, wheres, asc, desc,
  table,

  expr,
  relation, relation',

  query, query', queryMaybe, queryMaybe', from,

  PrimeRelation, Relation,

  inner', left', right', full',
  inner, left, right, full,

  toSQL,

  toSubQuery,

  nested, width
  ) where

import Prelude hiding (product)
import Control.Monad (liftM, ap)
import Data.Functor.Identity (Identity(Identity))
import Control.Monad.Trans.State (State, StateT(StateT), runState, modify)
import Control.Applicative (Applicative (pure, (<*>)))

import Database.Relational.Query.Internal.Context
  (Context, Order(Asc, Desc), OrderBys, primeContext, nextAlias, updateProduct, composeSQL)
import qualified Database.Relational.Query.Internal.Context as Context

import Database.Relational.Query.Internal.AliasId (AliasId, Qualified)
import qualified Database.Relational.Query.Internal.AliasId as AliasId

import Database.Relational.Query.Table (Table)

import Database.Relational.Query.Expr (Expr)

import Database.Relational.Query.Internal.Product
  (NodeAttr(Just', Maybe), QueryProductNode, growProduct, restrictProduct)

import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Projectable
  (Projectable(project), PlaceHolders, addPlaceHolders, projectZip)
import Database.Relational.Query.ProjectableExtended
  (ProjectableGeneralizedZip (generalizedZip))

import Database.Relational.Query.Sub (SubQuery)
import qualified Database.Relational.Query.Sub as SubQuery


newtype QueryJoin a =
  QueryJoin { queryJoinState :: State Context a }

runQueryJoin :: QueryJoin a -> Context -> (a, Context)
runQueryJoin = runState . queryJoinState

queryJoin :: (Context -> (a, Context)) -> QueryJoin a
queryJoin =  QueryJoin . StateT . (Identity .)

runQueryPrime :: QueryJoin a -> (a, Context)
runQueryPrime q = runQueryJoin q $ primeContext

newAlias :: QueryJoin AliasId
newAlias =  queryJoin nextAlias

updateContext :: (Context -> Context) -> QueryJoin ()
updateContext =  QueryJoin . modify

updateJoinRestriction :: Expr Bool -> QueryJoin ()
updateJoinRestriction e = updateContext (updateProduct d)  where
  d  Nothing  = error "on: product is empty!"
  d (Just pt) = restrictProduct pt e

updateRestriction :: Expr Bool -> QueryJoin ()
updateRestriction e = updateContext (Context.addRestriction e)

takeProduct :: QueryJoin (Maybe QueryProductNode)
takeProduct =  queryJoin Context.takeProduct

restoreLeft :: QueryProductNode -> NodeAttr -> QueryJoin ()
restoreLeft pL naR = updateContext $ Context.restoreLeft pL naR

updateOrderBy :: Order -> Expr t -> QueryJoin ()
updateOrderBy order e = updateContext (Context.updateOrderBy order e)

takeOrderBys :: QueryJoin OrderBys
takeOrderBys =  queryJoin Context.takeOrderBys

restoreLowOrderBys :: OrderBys -> QueryJoin ()
restoreLowOrderBys ros = updateContext (Context.restoreLowOrderBys ros)

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


table :: Table r -> Relation r
table =  SubQuery . SubQuery.fromTable

expr :: Projection ft -> Expr ft
expr =  project


instance Monad QueryJoin where
  return      = QueryJoin . return
  q0 >>= f    = QueryJoin $ queryJoinState q0 >>= queryJoinState . f

instance Functor QueryJoin where
  fmap = liftM

instance Applicative QueryJoin where
  pure  = return
  (<*>) = ap


qualify :: rel -> QueryJoin (Qualified rel)
qualify rel =
  do n <- newAlias
     return $ AliasId.qualify rel n

subQueryWithAttr :: NodeAttr -> SubQuery -> QueryJoin (Projection t)
subQueryWithAttr attr sub = do
  qsub <- qualify sub
  updateContext (updateProduct (`growProduct` (attr, qsub)))
  return $ Projection.fromQualifiedSubQuery qsub

unsafeMergeAnother :: NodeAttr -> QueryJoin a -> QueryJoin a
unsafeMergeAnother naR qR = do
  ros   <- takeOrderBys
  mayPL <- takeProduct
  v     <- qR
  maybe (return ()) (\pL -> restoreLeft pL naR) mayPL
  restoreLowOrderBys ros
  return v

queryMergeWithAttr :: NodeAttr -> QueryJoin (Projection r) -> QueryJoin (Projection r)
queryMergeWithAttr =  unsafeMergeAnother

queryWithAttr :: NodeAttr -> PrimeRelation p r -> QueryJoin (PlaceHolders p, Projection r)
queryWithAttr attr = addPlaceHolders . d where
  d (SubQuery sub)    = subQueryWithAttr attr sub
  d (PrimeRelation q) = queryMergeWithAttr attr q

query' :: PrimeRelation p r -> QueryJoin (PlaceHolders p, Projection r)
query' =  queryWithAttr Just'

query :: Relation r -> QueryJoin (Projection r)
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

from :: Table r -> Relation r
from =  relation . query . table


join' :: ProjectableGeneralizedZip pa pb pc
      => (qa -> QueryJoin (PlaceHolders pa, Projection a))
      -> (qb -> QueryJoin (PlaceHolders pb, Projection b))
      -> qa
      -> qb
      -> PrimeRelation pc (a, b)
join' qL qR r0 r1 = relation' $ do
  (ph0, pj0) <- qL r0
  (ph1, pj1) <- qR r1
  return $ (ph0 `generalizedZip` ph1, pj0 `projectZip` pj1)

inner' :: ProjectableGeneralizedZip pa pb pc
       => PrimeRelation pa a
       -> PrimeRelation pb b
       -> PrimeRelation pc (a, b)
inner' =  join' query' query'

left'  :: ProjectableGeneralizedZip pa pb pc
       => PrimeRelation pa a
       -> PrimeRelation pb b
       -> PrimeRelation pc (a, Maybe b)
left'  =  join' query' queryMaybe'

right' :: ProjectableGeneralizedZip pa pb pc
       => PrimeRelation pa a
       -> PrimeRelation pb b
       -> PrimeRelation pc(Maybe a, b)
right' =  join' queryMaybe' query'

full'  :: ProjectableGeneralizedZip pa pb pc
       => PrimeRelation pa a
       -> PrimeRelation pb b
       -> PrimeRelation pc (Maybe a, Maybe b)
full'  =  join' queryMaybe' queryMaybe'

join :: (qa -> QueryJoin (Projection a))
     -> (qb -> QueryJoin (Projection b))
     -> qa
     -> qb
     -> Relation (a, b)
join qL qR r0 r1 = relation $ do
  pj0 <- qL r0
  pj1 <- qR r1
  return $ pj0 `projectZip` pj1

inner :: Relation a
      -> Relation b
      -> Relation (a, b)
inner =  join query query

left  :: Relation a
      -> Relation b
      -> Relation (a, Maybe b)
left  =  join query queryMaybe

right :: Relation a
      -> Relation b
      -> Relation (Maybe a, b)
right =  join queryMaybe query

full  :: Relation a
      -> Relation b
      -> Relation (Maybe a, Maybe b)
full  =  join queryMaybe queryMaybe

infix 8 `inner'`, `left'`, `right'`, `full'`, `inner`, `left`, `right`, `full`

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

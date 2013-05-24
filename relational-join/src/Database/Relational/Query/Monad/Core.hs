{-# LANGUAGE FlexibleInstances #-}

module Database.Relational.Query.Monad.Core (
  QueryJoin,

  on, wheres, asc, desc,

  expr,

  unsafeSubQueryWithAttr,

  unsafeQueryMergeWithAttr,

  toSQL,

  toSubQuery,
  ) where

import Prelude hiding (product)
import Control.Monad (liftM, ap)
import Control.Monad.Trans.State (State, state, runState, modify)
import Control.Applicative (Applicative (pure, (<*>)))

import Database.Relational.Query.Internal.Context
  (Context, Order(Asc, Desc), OrderBys, primeContext, nextAlias, updateProduct, composeSQL)
import qualified Database.Relational.Query.Internal.Context as Context

import Database.Relational.Query.Internal.AliasId (AliasId, Qualified)
import qualified Database.Relational.Query.Internal.AliasId as AliasId

import Database.Relational.Query.Expr (Expr)

import Database.Relational.Query.Internal.Product
  (NodeAttr, QueryProductNode, growProduct, restrictProduct)

import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Projectable (Projectable(project))

import Database.Relational.Query.Sub (SubQuery)
import qualified Database.Relational.Query.Sub as SubQuery


newtype QueryJoin a =
  QueryJoin { queryJoinState :: State Context a }

runQueryJoin :: QueryJoin a -> Context -> (a, Context)
runQueryJoin =  runState . queryJoinState

queryJoin :: (Context -> (a, Context)) -> QueryJoin a
queryJoin =  QueryJoin . state

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

unsafeSubQueryWithAttr :: NodeAttr -> SubQuery -> QueryJoin (Projection t)
unsafeSubQueryWithAttr attr sub = do
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

unsafeQueryMergeWithAttr :: NodeAttr -> QueryJoin (Projection r) -> QueryJoin (Projection r)
unsafeQueryMergeWithAttr =  unsafeMergeAnother

toSQL :: QueryJoin (Projection r) -> String
toSQL =  uncurry composeSQL . runQueryPrime

instance Show (QueryJoin (Projection r)) where
  show = toSQL

toSubQuery :: QueryJoin (Projection r) -> SubQuery
toSubQuery qp = SubQuery.subQuery (composeSQL pj c) (Projection.width pj)  where
  (pj, c) = runQueryPrime qp

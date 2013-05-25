{-# LANGUAGE FlexibleInstances #-}

module Database.Relational.Query.Monad.Core (
  QueryCore,

  expr,

  unsafeSubQueryWithAttr,
  unsafeQueryMergeWithAttr,

  expandSQL
  ) where

import Prelude hiding (product)
import Control.Monad (liftM, ap)
import Control.Monad.Trans.State (State, state, runState, modify)
import Control.Applicative (Applicative (pure, (<*>)))

import Database.Relational.Query.Internal.Context
  (Context, primeContext, nextAlias, updateProduct, composeSQL)
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

import Database.Relational.Query.Monad.Class (MonadQuery(on, wheres))
import Database.Relational.Query.Monad.Unsafe (UnsafeMonadQuery(unsafeMergeAnotherQuery))

newtype QueryCore a =
  QueryCore { queryState :: State Context a }

runQueryCore :: QueryCore a -> Context -> (a, Context)
runQueryCore =  runState . queryState

queryCore :: (Context -> (a, Context)) -> QueryCore a
queryCore =  QueryCore . state

runQueryPrime :: QueryCore a -> (a, Context)
runQueryPrime q = runQueryCore q primeContext

newAlias :: QueryCore AliasId
newAlias =  queryCore nextAlias

updateContext :: (Context -> Context) -> QueryCore ()
updateContext =  QueryCore . modify


updateJoinRestriction :: Expr Bool -> QueryCore ()
updateJoinRestriction e = updateContext (updateProduct d)  where
  d  Nothing  = error "on: product is empty!"
  d (Just pt) = restrictProduct pt e

updateRestriction :: Expr Bool -> QueryCore ()
updateRestriction e = updateContext (Context.addRestriction e)

takeProduct :: QueryCore (Maybe QueryProductNode)
takeProduct =  queryCore Context.takeProduct

restoreLeft :: QueryProductNode -> NodeAttr -> QueryCore ()
restoreLeft pL naR = updateContext $ Context.restoreLeft pL naR

expr :: Projection ft -> Expr ft
expr =  project


instance Monad QueryCore where
  return      = QueryCore . return
  q0 >>= f    = QueryCore $ queryState q0 >>= queryState . f

instance Functor QueryCore where
  fmap = liftM

instance Applicative QueryCore where
  pure  = return
  (<*>) = ap

instance MonadQuery QueryCore where
  on =  updateJoinRestriction
  wheres =  updateRestriction

qualify :: rel -> QueryCore (Qualified rel)
qualify rel =
  do n <- newAlias
     return $ AliasId.qualify rel n

unsafeSubQueryWithAttr :: NodeAttr -> SubQuery -> QueryCore (Projection t)
unsafeSubQueryWithAttr attr sub = do
  qsub <- qualify sub
  updateContext (updateProduct (`growProduct` (attr, qsub)))
  return $ Projection.fromQualifiedSubQuery qsub

unsafeMergeAnother :: NodeAttr -> QueryCore a -> QueryCore a
unsafeMergeAnother naR qR = do
  mayPL <- takeProduct
  v     <- qR
  maybe (return ()) (\pL -> restoreLeft pL naR) mayPL
  return v

unsafeQueryMergeWithAttr :: NodeAttr -> QueryCore (Projection r) -> QueryCore (Projection r)
unsafeQueryMergeWithAttr =  unsafeMergeAnother

instance UnsafeMonadQuery QueryCore where
  unsafeMergeAnotherQuery = unsafeQueryMergeWithAttr

expandSQL :: QueryCore (Projection r, st) -> ((String, Projection r), st)
expandSQL qp = ((composeSQL pj c, pj), st)  where
  ((pj, st), c) = runQueryPrime qp

instance Show (QueryCore (Projection r)) where
  show = fst . fst . expandSQL . fmap (\x -> (,) x ())

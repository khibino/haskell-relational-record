module Database.Relational.Query.Monad.Trans.Join (
  QueryJoin, join', expandSQL,

  updateContext,

  updateJoinRestriction, updateRestriction
  ) where

import Prelude hiding (product)
import Control.Monad (liftM, ap)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State (modify, StateT, runStateT)
import Control.Applicative (Applicative (pure, (<*>)))

import Database.Relational.Query.Internal.Context
  (Context, primeContext, updateProduct)
import qualified Database.Relational.Query.Internal.Context as Context
import Database.Relational.Query.Projection (Projection)

import Database.Relational.Query.Expr (Expr, fromTriBool)

import Database.Relational.Query.Internal.Product (restrictProduct)


newtype QueryJoin m a =
  QueryJoin { queryState :: StateT Context m a }

runQueryJoin :: QueryJoin m a -> Context -> m (a, Context)
runQueryJoin =  runStateT . queryState

runQueryPrime :: QueryJoin m a -> m (a, Context)
runQueryPrime q = runQueryJoin q primeContext

instance MonadTrans QueryJoin where
  lift = QueryJoin . lift

join' :: Monad m => m a -> QueryJoin m a
join' =  lift

updateContext :: Monad m => (Context -> Context) -> QueryJoin m ()
updateContext =  QueryJoin . modify


updateJoinRestriction :: Monad m => Expr (Maybe Bool) -> QueryJoin m ()
updateJoinRestriction e = updateContext (updateProduct d)  where
  d  Nothing  = error "on: product is empty!"
  d (Just pt) = restrictProduct pt (fromTriBool e)

updateRestriction :: Monad m => Expr (Maybe Bool) -> QueryJoin m ()
updateRestriction e = updateContext (Context.addRestriction e)

-- takeProduct :: QueryJoin (Maybe QueryProductNode)
-- takeProduct =  queryCore Context.takeProduct

-- restoreLeft :: QueryProductNode -> NodeAttr -> QueryJoin ()
-- restoreLeft pL naR = updateContext $ Context.restoreLeft pL naR

instance Monad m => Monad (QueryJoin m) where
  return      = QueryJoin . return
  q0 >>= f    = QueryJoin $ queryState q0 >>= queryState . f

instance Monad m => Functor (QueryJoin m) where
  fmap = liftM

instance Monad m => Applicative (QueryJoin m) where
  pure  = return
  (<*>) = ap

expandSQL :: Monad m => QueryJoin m (Projection r, t) -> m ((String, Projection r), t)
expandSQL qp = do
  ((pj, st), c) <- runQueryPrime qp
  return ((Context.composeSQL pj c, pj), st)

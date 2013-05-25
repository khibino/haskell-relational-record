{-# LANGUAGE KindSignatures #-}

module Database.Relational.Query.Monad.Ordering (
  Orderings, orderings, OrderedQuery,

  unsafeMergeAnotherOrderBys,

  asc, desc,

  appendOrderBys
  ) where

import Prelude hiding (Ordering)
import Control.Monad (liftM, ap)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State (StateT, runStateT, modify, state)
import Control.Applicative (Applicative (pure, (<*>)))
import Control.Arrow (second)

import Database.Relational.Query.Internal.Context
  (Order(Asc, Desc), OrderBys, OrderingContext, primeOrderingContext)
import qualified Database.Relational.Query.Internal.Context as Context
import Database.Relational.Query.Internal.Product (NodeAttr)

import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection

import Database.Relational.Query.Monad.Class (MonadQuery)
import qualified Database.Relational.Query.Monad.Class as MonadQuery
import Database.Relational.Query.Monad.Unsafe (UnsafeMonadQuery)
import qualified Database.Relational.Query.Monad.Unsafe as UnsafeMonadQuery

newtype Orderings (p :: * -> *) m a =
  Orderings { orderingState :: StateT OrderingContext m a }

runOrderings :: Orderings p m a -> OrderingContext -> m (a, OrderingContext)
runOrderings =  runStateT . orderingState

runOrderingsPrime :: Orderings p m a -> m (a, OrderingContext)
runOrderingsPrime q = runOrderings q $ primeOrderingContext

instance MonadTrans (Orderings p) where
  lift = Orderings . lift

orderings :: Monad m => m a -> Orderings p m a
orderings =  lift

instance Monad m => Monad (Orderings p m) where
  return      = Orderings . return
  q0 >>= f    = Orderings $ orderingState q0 >>= orderingState . f

instance Monad m => Functor (Orderings p m) where
  fmap = liftM

instance Monad m => Applicative (Orderings p m) where
  pure  = return
  (<*>) = ap

instance MonadQuery m => MonadQuery (Orderings p m) where
  on     =  orderings . MonadQuery.on
  wheres =  orderings . MonadQuery.wheres

type OrderedQuery p m r = Orderings p m (p r)

class OrderingTerms p where
  orderTerms :: p t -> [String]

instance OrderingTerms Projection where
  orderTerms = Projection.columns

updateOrderingContext :: Monad m => (OrderingContext -> OrderingContext) -> Orderings p m ()
updateOrderingContext =  Orderings . modify

updateOrderBys :: (Monad m, OrderingTerms p) => Order -> p t -> Orderings p m ()
updateOrderBys order p = updateOrderingContext (\c -> foldl update c (orderTerms p))  where
  update = flip (Context.updateOrderBy order)

takeOrderBys :: Monad m => Orderings p m OrderBys
takeOrderBys =  Orderings $ state Context.takeOrderBys

restoreLowOrderBys :: Monad m => Context.OrderBys -> Orderings p m ()
restoreLowOrderBys ros = updateOrderingContext $ Context.restoreLowOrderBys ros

unsafeMergeAnotherOrderBys :: UnsafeMonadQuery m
                           => NodeAttr
                           -> Orderings p m (Projection r)
                           -> Orderings p m (Projection r)
unsafeMergeAnotherOrderBys naR qR = do
  ros   <- takeOrderBys
  let qR' = fst `liftM` runOrderingsPrime qR
  v     <- lift $ UnsafeMonadQuery.unsafeMergeAnotherQuery naR qR'
  restoreLowOrderBys ros
  return v

asc  :: (Monad m, OrderingTerms p) => p t -> Orderings p m ()
asc  =  updateOrderBys Asc

desc :: (Monad m, OrderingTerms p) => p t -> Orderings p m ()
desc =  updateOrderBys Desc

appendOrderBys' :: OrderingContext -> String -> String
appendOrderBys' c = (++ d (Context.composeOrderBys c))  where
  d "" = ""
  d s  = ' ' : s

appendOrderBys :: Monad m => Orderings p m a -> m (a, String -> String)
appendOrderBys q = second appendOrderBys' `liftM` runOrderingsPrime q

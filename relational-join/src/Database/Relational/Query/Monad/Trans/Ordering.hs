{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Database.Relational.Query.Monad.Trans.Ordering
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines monad transformer which lift
-- from 'MonadQuery' into query with ordering.
module Database.Relational.Query.Monad.Trans.Ordering (
  -- * Transformer into query with ordering
  Orderings, orderings, OrderedQuery, OrderingTerms,

  -- * API of query with ordering
  asc, desc,

  -- * Result SQL order-by clause
  extractOrderBys,
  OrderByAppend, appendOrderBy
  ) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State (StateT, runStateT, modify)
import Control.Applicative (Applicative, (<$>))
import Control.Arrow (second)

import Database.Relational.Query.Monad.Trans.StateAppend (Append, append, liftToString)
import Database.Relational.Query.Monad.Trans.OrderingState
  (Order(Asc, Desc), OrderingContext, primeOrderingContext, updateOrderBy, composeOrderBys)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Aggregation (Aggregation)
import qualified Database.Relational.Query.Aggregation as Aggregation

import Database.Relational.Query.Monad.Class
  (MonadRestrict(..), MonadQuery(..), MonadAggregate(..))


-- | 'StateT' type to accumulate ordering context.
--   Type 'p' is ordering term projection type.
newtype Orderings (p :: * -> *) m a =
  Orderings { orderingState :: StateT OrderingContext m a }
  deriving (MonadTrans, Monad, Functor, Applicative)

-- | Run 'Orderings' to expand context state
runOrderings :: Orderings p m a        -- ^ Context to expand
             -> OrderingContext        -- ^ Initial context
             -> m (a, OrderingContext) -- ^ Expanded result
runOrderings =  runStateT . orderingState

-- | Run 'Orderings' with primary empty context to expand context state.
runOrderingsPrime :: Orderings p m a        -- ^ Context to expand
                  -> m (a, OrderingContext) -- ^ Expanded result
runOrderingsPrime q = runOrderings q $ primeOrderingContext

-- | Lift to 'Orderings'.
orderings :: Monad m => m a -> Orderings p m a
orderings =  lift

-- | 'MonadRestrict' with ordering.
instance MonadRestrict m => MonadRestrict (Orderings p m) where
  restrictContext =  orderings . restrictContext

-- | 'MonadQuery' with ordering.
instance MonadQuery m => MonadQuery (Orderings p m) where
  restrictJoin  =  orderings . restrictJoin
  unsafeSubQuery na       = orderings . unsafeSubQuery na
  -- unsafeMergeAnotherQuery = unsafeMergeAnotherOrderBys

-- | 'MonadAggregate' with ordering.
instance MonadAggregate m => MonadAggregate (Orderings p m) where
  aggregateKey = orderings . aggregateKey
  restrictAggregatedQuery = orderings . restrictAggregatedQuery

-- | OrderedQuery type synonym. Projection must be the same as 'Orderings' type parameter 'p'
type OrderedQuery p m r = Orderings p m (p r)

-- | Ordering term projection type interface.
class OrderingTerms p where
  orderTerms :: p t -> [String]

-- | 'Projection' is ordering term.
instance OrderingTerms Projection where
  orderTerms = Projection.columns

-- | 'Aggregation' is ordering term.
instance OrderingTerms Aggregation where
  orderTerms = Projection.columns . Aggregation.unsafeProjection

-- | Unsafely update ordering context.
updateOrderingContext :: Monad m => (OrderingContext -> OrderingContext) -> Orderings p m ()
updateOrderingContext =  Orderings . modify

-- | Add ordering terms.
updateOrderBys :: (Monad m, OrderingTerms p)
               => Order            -- ^ Order direction
               -> p t              -- ^ Ordering terms to add
               -> Orderings p m () -- ^ Result context with ordering
updateOrderBys order p = updateOrderingContext (\c -> foldl update c (orderTerms p))  where
  update = flip (updateOrderBy order)

{-
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
  let qR' = fst <$> runOrderingsPrime qR
  v     <- lift $ unsafeMergeAnotherQuery naR qR'
  restoreLowOrderBys ros
  return v
-}


-- | Add ascendant ordering term.
asc  :: (Monad m, OrderingTerms p)
     => p t              -- ^ Ordering terms to add
     -> Orderings p m () -- ^ Result context with ordering
asc  =  updateOrderBys Asc

-- | Add descendant ordering term.
desc :: (Monad m, OrderingTerms p)
     => p t              -- ^ Ordering terms to add
     -> Orderings p m () -- ^ Result context with ordering
desc =  updateOrderBys Desc

-- | ORDER BY clause appending function.
type OrderByAppend = Append OrderingContext

-- | Run 'Orderings' to get ORDER BY clause appending function.
extractOrderBys :: (Monad m, Functor m)
               => Orderings p m a      -- ^ 'Orderings' to run
               -> m (a, OrderByAppend) -- ^ Query result and order-by appending function.
extractOrderBys q = second (liftToString composeOrderBys) <$> runOrderingsPrime q

-- | Run ORDER BY clause append.
appendOrderBy :: OrderByAppend -> String -> String
appendOrderBy =  append

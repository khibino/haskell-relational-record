{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
-- from query into query with ordering.
module Database.Relational.Query.Monad.Trans.Ordering (
  -- * Transformer into query with ordering
  Orderings, orderings, OrderedQuery, OrderingTerms,

  -- * API of query with ordering
  asc, desc,

  -- * Result
  extractOrderingTerms
  ) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State (StateT, runStateT, modify)
import Control.Applicative (Applicative, (<$>))
import Control.Arrow (second, (>>>))

import Database.Relational.Query.Component
  (Order(Asc, Desc), OrderColumn, OrderingTerm, OrderingTerms)
import Database.Relational.Query.Monad.Trans.ListState
  (TermsContext, primeTermsContext, appendTerm, termsList)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection

import Database.Relational.Query.Monad.Class
  (MonadRestrict(..), MonadQuery(..), MonadAggregate(..), MonadPartition(..))


type OrderingContext = TermsContext OrderingTerm

-- | 'StateT' type to accumulate ordering context.
--   Type 'c' is ordering term projection context type.
newtype Orderings c m a =
  Orderings { orderingState :: StateT OrderingContext m a }
  deriving (MonadTrans, Monad, Functor, Applicative)

-- | Run 'Orderings' to expand context state.
runOrderings :: Orderings c m a        -- ^ Context to expand
             -> OrderingContext        -- ^ Initial context
             -> m (a, OrderingContext) -- ^ Expanded result
runOrderings =  runStateT . orderingState

-- | Run 'Orderings' with primary empty context to expand context state.
runOrderingsPrime :: Orderings c m a        -- ^ Context to expand
                  -> m (a, OrderingContext) -- ^ Expanded result
runOrderingsPrime q = runOrderings q $ primeTermsContext

-- | Lift to 'Orderings'.
orderings :: Monad m => m a -> Orderings c m a
orderings =  lift

-- | 'MonadRestrict' with ordering.
instance MonadRestrict rc m => MonadRestrict rc (Orderings c m) where
  restrictContext = orderings . restrictContext

-- | 'MonadQuery' with ordering.
instance MonadQuery m => MonadQuery (Orderings c m) where
  restrictJoin  =  orderings . restrictJoin
  unsafeSubQuery na       = orderings . unsafeSubQuery na
  -- unsafeMergeAnotherQuery = unsafeMergeAnotherOrderBys

-- | 'MonadAggregate' with ordering.
instance MonadAggregate m => MonadAggregate (Orderings c m) where
  unsafeAddAggregateElement = orderings . unsafeAddAggregateElement

-- | 'MonadPartition' with ordering.
instance MonadPartition m => MonadPartition (Orderings c m) where
  unsafeAddPartitionKey = orderings . unsafeAddPartitionKey

-- | OrderedQuery type synonym. Projection must be the same as 'Orderings' type parameter 'p'
type OrderedQuery c m r = Orderings c m (Projection c r)

-- | Ordering term projection type interface.
class ProjectableOrdering p where
  orderTerms :: p t -> [OrderColumn]

-- | 'Projection' is ordering term.
instance ProjectableOrdering (Projection c) where
  orderTerms = Projection.columns

-- | Unsafely update ordering context.
updateOrderingContext :: Monad m => (OrderingContext -> OrderingContext) -> Orderings p m ()
updateOrderingContext =  Orderings . modify

-- | Add ordering terms.
updateOrderBys :: (Monad m, ProjectableOrdering (Projection c))
               => Order            -- ^ Order direction
               -> Projection c t   -- ^ Ordering terms to add
               -> Orderings c m () -- ^ Result context with ordering
updateOrderBys order p = updateOrderingContext . foldr (>>>) id $ updates  where
  updates = curry appendTerm order `map` orderTerms p

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
asc :: (Monad m, ProjectableOrdering (Projection c))
    => Projection c t   -- ^ Ordering terms to add
    -> Orderings c m () -- ^ Result context with ordering
asc  =  updateOrderBys Asc

-- | Add descendant ordering term.
desc :: (Monad m, ProjectableOrdering (Projection c))
     => Projection c t   -- ^ Ordering terms to add
     -> Orderings c m () -- ^ Result context with ordering
desc =  updateOrderBys Desc

-- | Run 'Orderings' to get 'OrderingTerms'
extractOrderingTerms :: (Monad m, Functor m) => Orderings c m a -> m (a, OrderingTerms)
extractOrderingTerms q = second termsList <$> runOrderingsPrime q

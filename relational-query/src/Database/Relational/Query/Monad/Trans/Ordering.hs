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
  orderBy, asc, desc,

  -- * Result
  extractOrderingTerms
  ) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Control.Applicative (Applicative, pure, (<$>))
import Control.Arrow (second)
import Data.DList (DList, toList)

import Database.Relational.Query.Component
  (Order(Asc, Desc), OrderColumn, OrderingTerm, OrderingTerms)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection

import Database.Relational.Query.Monad.Class
  (MonadRestrict(..), MonadQuery(..), MonadAggregate(..), MonadPartition(..))


-- | Type to accumulate ordering context.
--   Type 'c' is ordering term projection context type.
newtype Orderings c m a =
  Orderings (WriterT (DList OrderingTerm) m a)
  deriving (MonadTrans, Monad, Functor, Applicative)

-- | Lift to 'Orderings'.
orderings :: Monad m => m a -> Orderings c m a
orderings =  lift

-- | 'MonadRestrict' with ordering.
instance MonadRestrict rc m => MonadRestrict rc (Orderings c m) where
  restrictContext = orderings . restrictContext

-- | 'MonadQuery' with ordering.
instance MonadQuery m => MonadQuery (Orderings c m) where
  setDuplication     = orderings . setDuplication
  restrictJoin       = orderings . restrictJoin
  unsafeSubQuery na  = orderings . unsafeSubQuery na

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

-- | Add ordering terms.
updateOrderBys :: (Monad m, ProjectableOrdering (Projection c))
               => Order            -- ^ Order direction
               -> Projection c t   -- ^ Ordering terms to add
               -> Orderings c m () -- ^ Result context with ordering
updateOrderBys order p = Orderings . mapM_ tell $ terms  where
  terms = curry pure order `map` orderTerms p

-- | Add ordering terms.
orderBy :: (Monad m, ProjectableOrdering (Projection c))
        => Projection c t   -- ^ Ordering terms to add
        -> Order            -- ^ Order direction
        -> Orderings c m () -- ^ Result context with ordering
orderBy = flip updateOrderBys

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
extractOrderingTerms (Orderings oc) = second toList <$> runWriterT oc

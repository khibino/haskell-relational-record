{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Monad.Trans.Ordering
-- Copyright   : 2013-2018 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines monad transformer which lift
-- from query into query with ordering.
module Database.Relational.Monad.Trans.Ordering (
  -- * Transformer into query with ordering
  Orderings, orderings,

  -- * API of query with ordering
  orderByNoPh', orderByNoPh, ascNoPh, descNoPh,

  -- * Result
  extractOrderingTerms
  ) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Control.Applicative (Applicative, pure, (<$>))
import Control.Arrow (second)
import Data.DList (DList, toList)

import Database.Relational.SqlSyntax
  (Order (..), Nulls (..), OrderingTerm, Record, untypeRecord)

import Database.Relational.Monad.Class
  (MonadQualify (..), MonadRestrict(..), MonadQuery(..), MonadAggregate(..), MonadPartition(..))


-- | Type to accumulate ordering context.
--   Type 'c' is ordering term record context type.
newtype Orderings c m a =
  Orderings (WriterT (DList OrderingTerm) m a)
  deriving (MonadTrans, Monad, Functor, Applicative)

-- | Lift to 'Orderings'.
orderings :: Monad m => m a -> Orderings c m a
orderings =  lift

-- | 'MonadRestrict' with ordering.
instance MonadRestrict rc m => MonadRestrict rc (Orderings c m) where
  restrictNoPh = orderings . restrictNoPh

-- | 'MonadQualify' with ordering.
instance MonadQualify q m => MonadQualify q (Orderings c m) where
  liftQualify = orderings . liftQualify

-- | 'MonadQuery' with ordering.
instance MonadQuery m => MonadQuery (Orderings c m) where
  setDuplication   = orderings . setDuplication
  restrictJoinNoPh = orderings . restrictJoinNoPh
  queryNoPh'       = orderings . queryNoPh'
  queryMaybeNoPh'  = orderings . queryMaybeNoPh'

-- | 'MonadAggregate' with ordering.
instance MonadAggregate m => MonadAggregate (Orderings c m) where
  groupByNoPh  = orderings . groupByNoPh
  groupByNoPh' = orderings . groupByNoPh'

-- | 'MonadPartition' with ordering.
instance MonadPartition c m => MonadPartition c (Orderings c m) where
  partitionByNoPh = orderings . partitionByNoPh

-- | Add ordering terms.
updateOrderBys :: Monad m
               => (Order, Maybe Nulls) -- ^ Order direction
               -> Record c t       -- ^ Ordering terms to add
               -> Orderings c m ()     -- ^ Result context with ordering
updateOrderBys opair p = Orderings . mapM_ tell $ terms  where
  terms = curry pure opair `map` untypeRecord p

-- | Add ordering terms with null ordering.
orderByNoPh' :: Monad m
             => Record c t   -- ^ Ordering terms to add
             -> Order            -- ^ Order direction
             -> Nulls            -- ^ Order of null
             -> Orderings c m () -- ^ Result context with ordering
orderByNoPh' p o n = updateOrderBys (o, Just n) p

-- | Add ordering terms.
orderByNoPh :: Monad m
            => Record c t   -- ^ Ordering terms to add
            -> Order        -- ^ Order direction
            -> Orderings c m () -- ^ Result context with ordering
orderByNoPh p o = updateOrderBys (o, Nothing) p

-- | Add ascendant ordering term.
ascNoPh :: Monad m
        => Record c t   -- ^ Ordering terms to add
        -> Orderings c m () -- ^ Result context with ordering
ascNoPh  =  updateOrderBys (Asc, Nothing)

-- | Add descendant ordering term.
descNoPh :: Monad m
         => Record c t   -- ^ Ordering terms to add
         -> Orderings c m () -- ^ Result context with ordering
descNoPh =  updateOrderBys (Desc, Nothing)

-- | Run 'Orderings' to get 'OrderingTerms'
extractOrderingTerms :: (Monad m, Functor m) => Orderings c m a -> m (a, [OrderingTerm])
extractOrderingTerms (Orderings oc) = second toList <$> runWriterT oc

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Database.Relational.Monad.Trans.Ordering
-- Copyright   : 2013-2017 Kei Hibino
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
  orderBy', orderBy, asc, desc,

  -- * Result
  extractOrderingTerms
  ) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Control.Applicative (Applicative, pure, (<$>))
import Control.Arrow (second)
import Data.DList (DList, toList)

import Database.Relational.ExtensibleRecord
import qualified Database.Relational.Monad.Trans.Placeholders as P
import Database.Relational.SqlSyntax
  (Order (..), Nulls (..), OrderingTerm, Record)

import qualified Database.Relational.Record as Record
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
  restrict = orderings . restrict

-- | 'MonadQualify' with ordering.
instance MonadQualify q m => MonadQualify q (Orderings c m) where
  liftQualify = orderings . liftQualify

-- | 'MonadQuery' with ordering.
instance MonadQuery m => MonadQuery (Orderings c m) where
  setDuplication                         = orderings . setDuplication
  restrictJoin                           = orderings . restrictJoin
  queryWithoutUpdatingPlaceholders'      = orderings . queryWithoutUpdatingPlaceholders'
  queryMaybeWithoutUpdatingPlaceholders' = orderings . queryMaybeWithoutUpdatingPlaceholders'

-- | 'MonadAggregate' with ordering.
instance MonadAggregate m => MonadAggregate (Orderings c m) where
  groupByWithoutUpdatingPlaceholders  = orderings . groupByWithoutUpdatingPlaceholders
  groupByWithoutUpdatingPlaceholders' = orderings . groupByWithoutUpdatingPlaceholders'

-- | 'MonadPartition' with ordering.
instance MonadPartition c m => MonadPartition c (Orderings c m) where
  partitionByWithoutUpdatingPlaceholders = orderings . partitionByWithoutUpdatingPlaceholders

-- | Add ordering terms.
updateOrderBys :: Monad m
               => (Order, Maybe Nulls) -- ^ Order direction
               -> Record (ExRecord '[]) (ExRecord ys) c t       -- ^ Ordering terms to add
               -> Orderings c m ()     -- ^ Result context with ordering
updateOrderBys opair p = Orderings . mapM_ tell $ terms  where
  terms = curry pure opair `map` Record.columns p

-- | Add ordering terms with null ordering.
orderBy' :: Monad m
         => Record (ExRecord '[]) (ExRecord ys) c t   -- ^ Ordering terms to add
         -> Order            -- ^ Order direction
         -> Nulls            -- ^ Order of null
         -> P.Placeholders (Orderings c m) (ExRecord xs) (ExRecord (xs ++ ys)) () -- ^ Result context with ordering
orderBy' p o n = P.addingPlaceholders p $ updateOrderBys (o, Just n) p

-- | Add ordering terms.
orderBy :: Monad m
        => Record (ExRecord '[]) (ExRecord ys) c t   -- ^ Ordering terms to add
        -> Order        -- ^ Order direction
        -> P.Placeholders (Orderings c m) (ExRecord xs) (ExRecord (xs ++ ys)) () -- ^ Result context with ordering
orderBy p o = P.addingPlaceholders p $ updateOrderBys (o, Nothing) p

-- | Add ascendant ordering term.
asc :: Monad m
    => Record (ExRecord '[]) (ExRecord ys) c t   -- ^ Ordering terms to add
    -> P.Placeholders (Orderings c m) (ExRecord xs) (ExRecord (xs ++ ys)) () -- ^ Result context with ordering
asc p =  P.addingPlaceholders p $ updateOrderBys (Asc, Nothing) p

-- | Add descendant ordering term.
desc :: Monad m
     => Record (ExRecord '[]) (ExRecord ys) c t   -- ^ Ordering terms to add
     -> P.Placeholders (Orderings c m) (ExRecord xs) (ExRecord (xs ++ ys)) () -- ^ Result context with ordering
desc p =  P.addingPlaceholders p $ updateOrderBys (Desc, Nothing) p

-- | Run 'Orderings' to get 'OrderingTerms'
extractOrderingTerms :: (Monad m, Functor m) => Orderings c m a -> m (a, [OrderingTerm])
extractOrderingTerms (Orderings oc) = second toList <$> runWriterT oc

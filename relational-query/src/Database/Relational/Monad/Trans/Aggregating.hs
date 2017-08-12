{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Monad.Trans.Aggregating
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines monad transformer which lift
-- from 'MonadQuery' into Aggregated query.
module Database.Relational.Monad.Trans.Aggregating
       ( -- * Transformer into aggregated query
         Aggregatings, aggregatings,

         AggregatingSetT, AggregatingSetListT, AggregatingPowerSetT, PartitioningSetT,

         -- * Result
         extractAggregateTerms,

         -- * Grouping sets support
         AggregatingSet, AggregatingPowerSet,  AggregatingSetList, PartitioningSet,
         key, key', set,
         bkey, rollup, cube, groupingSets,
       ) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Control.Applicative (Applicative, pure, (<$>))
import Control.Arrow (second)
import Data.DList (DList, toList)

import Data.Functor.Identity (Identity (runIdentity))

import Database.Relational.Internal.Sub (Record)
import Database.Relational.Internal.GroupingSQL
  (AggregateColumnRef, AggregateElem, aggregateColumnRef, AggregateSet, aggregateGroupingSet,
   AggregateBitKey, aggregatePowerKey, aggregateRollup, aggregateCube, aggregateSets,
   AggregateKey, aggregateKeyProjection, aggregateKeyElement, unsafeAggregateKey)

import Database.Relational.Context (Flat, Aggregated, Set, Power, SetList)
import qualified Database.Relational.Projection as Projection
import Database.Relational.Monad.Class
  (MonadQualify (..), MonadRestrict(..), MonadQuery(..), MonadAggregate(..), MonadPartition(..))


-- | Type to accumulate aggregating context.
--   Type 'ac' is aggregating-context type like aggregating key set building,
--   aggregating key sets set building and partition key set building.
--   Type 'at' is aggregating term type.
newtype Aggregatings ac at m a =
  Aggregatings (WriterT (DList at) m a)
  deriving (MonadTrans, Monad, Functor, Applicative)

-- | Lift to 'Aggregatings'.
aggregatings :: Monad m => m a -> Aggregatings ac at m a
aggregatings =  lift

-- | Context type building one grouping set.
type AggregatingSetT      = Aggregatings Set       AggregateElem

-- | Context type building grouping sets list.
type AggregatingSetListT  = Aggregatings SetList   AggregateSet

-- | Context type building power group set.
type AggregatingPowerSetT = Aggregatings Power     AggregateBitKey

-- | Context type building partition keys set.
type PartitioningSetT c   = Aggregatings c         AggregateColumnRef

-- | Aggregated 'MonadRestrict'.
instance MonadRestrict c m => MonadRestrict c (AggregatingSetT m) where
  restrict =  aggregatings . restrict

-- | Aggregated 'MonadQualify'.
instance MonadQualify q m => MonadQualify q (AggregatingSetT m) where
  liftQualify = aggregatings . liftQualify

-- | Aggregated 'MonadQuery'.
instance MonadQuery m => MonadQuery (AggregatingSetT m) where
  setDuplication     = aggregatings . setDuplication
  restrictJoin       = aggregatings . restrictJoin
  query'             = aggregatings . query'
  queryMaybe'        = aggregatings . queryMaybe'

unsafeAggregateWithTerm :: Monad m => at -> Aggregatings ac at m ()
unsafeAggregateWithTerm =  Aggregatings . tell . pure

aggregateKey :: Monad m => AggregateKey a -> Aggregatings ac AggregateElem m a
aggregateKey k = do
  unsafeAggregateWithTerm $ aggregateKeyElement k
  return $ aggregateKeyProjection k

-- | Aggregated query instance.
instance MonadQuery m => MonadAggregate (AggregatingSetT m) where
  groupBy p = do
    mapM_ unsafeAggregateWithTerm [ aggregateColumnRef col | col <- Projection.columns p]
    return $ Projection.unsafeToAggregated p
  groupBy'  = aggregateKey

-- | Partition clause instance
instance Monad m => MonadPartition c (PartitioningSetT c m) where
  partitionBy =  mapM_ unsafeAggregateWithTerm . Projection.columns

-- | Run 'Aggregatings' to get terms list.
extractAggregateTerms :: (Monad m, Functor m) => Aggregatings ac at m a -> m (a, [at])
extractAggregateTerms (Aggregatings ac) = second toList <$> runWriterT ac

extractTermList :: Aggregatings ac at Identity a -> (a, [at])
extractTermList =  runIdentity . extractAggregateTerms

-- | Context monad type to build single grouping set.
type AggregatingSet      = AggregatingSetT      Identity

-- | Context monad type to build grouping power set.
type AggregatingPowerSet = AggregatingPowerSetT Identity

-- | Context monad type to build grouping set list.
type AggregatingSetList  = AggregatingSetListT  Identity

-- | Context monad type to build partition keys set.
type PartitioningSet c   = PartitioningSetT c   Identity

-- | Specify key of single grouping set from Projection.
key :: Record Flat r
    -> AggregatingSet (Record Aggregated (Maybe r))
key p = do
  mapM_ unsafeAggregateWithTerm [ aggregateColumnRef col | col <- Projection.columns p]
  return . Projection.just $ Projection.unsafeToAggregated p

-- | Specify key of single grouping set.
key' :: AggregateKey a
     -> AggregatingSet a
key' = aggregateKey

-- | Finalize and specify single grouping set.
set :: AggregatingSet a
    -> AggregatingSetList a
set s = do
  let (p, c) = second aggregateGroupingSet . extractTermList $ s
  unsafeAggregateWithTerm c
  return p

-- | Specify key of rollup and cube power set.
bkey :: Record Flat r
     -> AggregatingPowerSet (Record Aggregated (Maybe r))
bkey p = do
  unsafeAggregateWithTerm . aggregatePowerKey $ Projection.columns p
  return . Projection.just $ Projection.unsafeToAggregated p

finalizePower :: ([AggregateBitKey] -> AggregateElem)
              -> AggregatingPowerSet a -> AggregateKey a
finalizePower finalize pow = unsafeAggregateKey . second finalize . extractTermList $ pow

-- | Finalize grouping power set as rollup power set.
rollup :: AggregatingPowerSet a -> AggregateKey a
rollup =  finalizePower aggregateRollup

-- | Finalize grouping power set as cube power set.
cube   :: AggregatingPowerSet a -> AggregateKey a
cube   =  finalizePower aggregateCube

-- | Finalize grouping set list.
groupingSets :: AggregatingSetList a -> AggregateKey a
groupingSets =  unsafeAggregateKey . second aggregateSets . extractTermList

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Monad.Trans.Aggregating
-- Copyright   : 2013-2018 Kei Hibino
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
import Control.Applicative (Applicative, (<$>))
import Control.Arrow (second)
import Data.DList (DList, fromList, toList, singleton)
import Data.Monoid (mempty)

import Data.Functor.Identity (Identity (runIdentity))

import Database.Relational.Internal.ContextType
  (Flat, Aggregated, Set, Power, SetList)
import Database.Relational.SqlSyntax
  (Record,
   AggregateColumnRef, AggregateElem, aggregateColumnRef, AggregateSet, aggregateGroupingSet,
   AggregateBitKey, aggregatePowerKey, aggregateRollup, aggregateCube, aggregateSets,
   AggregateKey, aggregateKeyRecord, aggregateKeyElement, unsafeAggregateKey,
   WithPlaceholderOffsetsT (WithPlaceholderOffsetsT), WithPlaceholderOffsets,
   runWithPlaceholderOffsetsT, withPlaceholderOffsets, appendPlaceholderOffsets,
   tupleFromPlaceholderOffsets, record,
   untypeRecord, untypeRecordWithPlaceholderOffsets, emptyPlaceholderOffsetsOfRecord,)

import qualified Database.Relational.Record as Record
import Database.Relational.Monad.Class
  (MonadQualify (..), MonadRestrict(..), MonadQuery(..), MonadAggregate(..), MonadPartition(..))


-- | Type to accumulate aggregating context.
--   Type 'ac' is aggregating-context type like aggregating key set building,
--   aggregating key sets set building and partition key set building.
--   Type 'at' is aggregating term type.
newtype Aggregatings ac at m a =
  Aggregatings { unAggregatings :: (WithPlaceholderOffsetsT (AggregatingsBase ac at m) a) }
  deriving (Monad, Functor, Applicative)

instance MonadTrans (Aggregatings ac at) where
  lift = Aggregatings . lift . lift

newtype AggregatingsBase ac at m a =
  AggregatingsBase { unAggregatingsBase :: (WriterT (DList at) m a) }
  deriving (MonadTrans, Monad, Functor, Applicative)

-- | Lift to 'Aggregatings'.
aggregatings :: Monad m => m a -> Aggregatings ac at m a
aggregatings =  lift

-- | Context type building one grouping set.
type AggregatingSetT      = Aggregatings     Set       AggregateElem

-- | Context type building one grouping set.
type AggregatingSetBaseT  = AggregatingsBase Set       AggregateElem

-- | Context type building grouping sets list.
type AggregatingSetListT  = AggregatingsBase SetList   AggregateSet

-- | Context type building power group set.
type AggregatingPowerSetT = AggregatingsBase Power     AggregateBitKey

-- | Context type building partition keys set.
type PartitioningSetT c   = Aggregatings     c         AggregateColumnRef

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
  query' ph          = aggregatings . query' ph
  queryMaybe' ph     = aggregatings . queryMaybe' ph

unsafeAggregateWithTerm :: Monad m => at -> AggregatingsBase ac at m ()
unsafeAggregateWithTerm =  AggregatingsBase . tell . singleton

unsafeAggregateWithTerms :: Monad m => [at] -> AggregatingsBase ac at m ()
unsafeAggregateWithTerms =  AggregatingsBase . tell . fromList

aggregateKey :: Monad m => AggregateKey a -> AggregatingsBase ac AggregateElem m a
aggregateKey k = do
  unsafeAggregateWithTerm $ aggregateKeyElement k
  return $ aggregateKeyRecord k

-- | Aggregated query instance.
instance MonadQuery m => MonadAggregate (AggregatingSetT m) where
  groupBy p = do
    let (ts, phs) = tupleFromPlaceholderOffsets $ untypeRecordWithPlaceholderOffsets p
    Aggregatings $ do
      lift . unsafeAggregateWithTerms $ map aggregateColumnRef ts
      appendPlaceholderOffsets phs
    return . emptyPlaceholderOffsetsOfRecord $ Record.unsafeToAggregated p
  groupBy' kr = do
    r <- Aggregatings . lift $ aggregateKey kr
    let (ts, phs) = tupleFromPlaceholderOffsets $ untypeRecordWithPlaceholderOffsets r
    Aggregatings $ appendPlaceholderOffsets phs
    return $ record mempty ts

-- | Partition clause instance
instance Monad m => MonadPartition c (PartitioningSetT c m) where
  partitionBy r = Aggregatings $ do
    lift $ unsafeAggregateWithTerms ts
    appendPlaceholderOffsets phs
   where
    (ts, phs) = tupleFromPlaceholderOffsets $ untypeRecordWithPlaceholderOffsets r

-- | Run 'Aggregatings' to get terms list.
extractAggregateTerms :: (Monad m, Functor m) => Aggregatings ac at m a -> m (a, WithPlaceholderOffsets [at])
extractAggregateTerms = fmap f . runWriterT . unAggregatingsBase . runWithPlaceholderOffsetsT . unAggregatings
 where
  f ((x, phs), ats) = (x, withPlaceholderOffsets phs $ toList ats)

extractAggregateTermsBase :: (Monad m, Functor m) => AggregatingsBase ac at m a -> m (a, [at])
extractAggregateTermsBase (AggregatingsBase ac) = second toList <$> runWriterT ac

extractTermList :: AggregatingsBase ac at Identity a -> (a, [at])
extractTermList =  runIdentity . extractAggregateTermsBase

-- | Context monad type to build single grouping set.
type AggregatingSet      = AggregatingSetBaseT  Identity

-- | Context monad type to build grouping power set.
type AggregatingPowerSet = AggregatingPowerSetT Identity

-- | Context monad type to build grouping set list.
type AggregatingSetList  = AggregatingSetListT  Identity

-- | Context monad type to build partition keys set.
type PartitioningSet c   = PartitioningSetT c   Identity

-- | Specify key of single grouping set from Record.
key :: Record Flat r
    -> AggregatingSet (Record Aggregated (Maybe r))
key p = do
  mapM_ unsafeAggregateWithTerm [ aggregateColumnRef col | col <- untypeRecord p]
  return . Record.just $ Record.unsafeToAggregated p

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
  unsafeAggregateWithTerm . aggregatePowerKey $ untypeRecord p
  return . Record.just $ Record.unsafeToAggregated p

finalizePower :: ([AggregateBitKey] -> AggregateElem)
              -> AggregatingPowerSet a -> AggregateKey a
finalizePower finalize pow = unsafeAggregateKey . second finalize $ extractTermList pow

-- | Finalize grouping power set as rollup power set.
rollup :: AggregatingPowerSet a -> AggregateKey a
rollup =  finalizePower aggregateRollup

-- | Finalize grouping power set as cube power set.
cube   :: AggregatingPowerSet a -> AggregateKey a
cube   =  finalizePower aggregateCube

-- | Finalize grouping set list.
groupingSets :: AggregatingSetList a -> AggregateKey a
groupingSets =  unsafeAggregateKey . second aggregateSets . extractTermList

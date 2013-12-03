{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Query.Monad.Trans.Aggregating
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines monad transformer which lift
-- from 'MonadQuery' into Aggregated query.
module Database.Relational.Query.Monad.Trans.Aggregating (
  -- * Transformer into aggregated query
  Aggregatings, aggregatings,

  AggregatingSetT, AggregatingSetListT, AggregatingPowerSetT,

  by',

  -- * Result
  extractAggregateTerms,

  key, key', set,
  AggregatingPowerSet, rollup, cube,
  AggregatingSetList, groupingSets
  ) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State (StateT, runStateT, modify)
import Control.Applicative (Applicative, (<$>))
import Control.Arrow (second)

import Data.Functor.Identity (Identity (runIdentity))

import Database.Relational.Query.Context (Flat, Aggregated, Set, Power, SetList)
import Database.Relational.Query.Component
  (AggregateElem, aggregateColumnRef, AggregateSet, aggregateGroupingSet, AggregateKey, aggregatePowerKey,
  aggregateRollup, aggregateCube, aggregateSets)
import Database.Relational.Query.Monad.Trans.ListState
  (TermsContext, primeTermsContext, appendTerm, termsList)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection

import Database.Relational.Query.Monad.Class
  (MonadRestrict(..), MonadQuery(..), MonadAggregate(..))


-- | 'StateT' type to accumulate aggregating context.
newtype Aggregatings ac at m a =
  Aggregatings { aggregatingState :: StateT (TermsContext at) m a }
  deriving (MonadTrans, Monad, Functor, Applicative)

-- | Run 'Aggregatings' to expand context state.
runAggregating :: Aggregatings ac at m a -- ^ Context to expand
               -> TermsContext at        -- ^ Initial context
               -> m (a, TermsContext at) -- ^ Expanded result
runAggregating =  runStateT . aggregatingState

-- | Run 'Aggregatings' with primary empty context to expand context state.
runAggregatingPrime :: Aggregatings ac at m a          -- ^ Context to expand
                    -> m (a, TermsContext at) -- ^ Expanded result
runAggregatingPrime =  (`runAggregating` primeTermsContext)

-- | Lift to 'Aggregatings'.
aggregatings :: Monad m => m a -> Aggregatings ac at m a
aggregatings =  lift

-- | Context type building one grouping set.
type AggregatingSetT      = Aggregatings Set     AggregateElem

-- | Context type building grouping sets list.
type AggregatingSetListT  = Aggregatings SetList AggregateSet

-- | Context type building power group set.
type AggregatingPowerSetT = Aggregatings Power   AggregateKey

-- | Aggregated 'MonadRestrict'.
instance MonadRestrict c m => MonadRestrict c (AggregatingSetT m) where
  restrictContext =  aggregatings . restrictContext

-- | Aggregated 'MonadQuery'.
instance MonadQuery m => MonadQuery (AggregatingSetT m) where
  restrictJoin  =  aggregatings . restrictJoin
  unsafeSubQuery na = aggregatings . unsafeSubQuery na

-- | Unsafely update aggregating context.
updateAggregatingContext :: Monad m => (TermsContext at -> TermsContext at) -> Aggregatings ac at m ()
updateAggregatingContext =  Aggregatings . modify

unsafeAggregateWithTerm :: Monad m => at -> Aggregatings ac at m ()
unsafeAggregateWithTerm =  updateAggregatingContext . appendTerm

-- | Aggregated query instance.
instance MonadQuery m => MonadAggregate (AggregatingSetT m) where
  unsafeAddAggregateElement = unsafeAggregateWithTerm

-- | Specify key of rollup and cube power set.
by' :: Monad m
    => Projection Flat r
    -> AggregatingPowerSetT m (Projection Aggregated (Maybe r))
by' p = do
  unsafeAggregateWithTerm . aggregatePowerKey $ Projection.columns p
  return . Projection.just $ Projection.unsafeToAggregated p

-- | Run 'Aggregatings' to get terms list.
extractAggregateTerms :: (Monad m, Functor m) => Aggregatings ac at m a -> m (a, [at])
extractAggregateTerms q = second termsList <$> runAggregatingPrime q


extractTermList :: Aggregatings ac at Identity a -> (a, [at])
extractTermList =  runIdentity . extractAggregateTerms

-- | Context monad type to build single grouping set.
type AggregatingSet      = AggregatingSetT      Identity

-- | Context monad type to build grouping power set.
type AggregatingPowerSet = AggregatingPowerSetT Identity

-- | Context monad type to build grouping set list.
type AggregatingSetList  = AggregatingSetListT  Identity

-- | Specify key of single grouping set from Projection.
key :: Monad m
    => Projection Flat r
    -> AggregatingSet (Projection Aggregated (Maybe r))
key p = do
  mapM_ unsafeAggregateWithTerm [ aggregateColumnRef col | col <- Projection.columns p]
  return . Projection.just $ Projection.unsafeToAggregated p

-- | Specify key of single grouping set.
key' :: Monad m
     => (a, AggregateElem)
     -> AggregatingSet a
key' (p, c) = do
  unsafeAggregateWithTerm c
  return p

-- | Finalize and specify single grouping set.
set :: Monad m
    => AggregatingSet a
    -> AggregatingSetList a
set s = do
  let (p, c) = second aggregateGroupingSet . extractTermList $ s
  unsafeAggregateWithTerm c
  return p

finalizePower :: ([AggregateKey] -> AggregateElem)
              -> AggregatingPowerSet a -> (a, AggregateElem)
finalizePower finalize pow = second finalize . extractTermList $ pow

-- | Finalize grouping power set as rollup power set.
rollup :: AggregatingPowerSet a -> (a, AggregateElem)
rollup =  finalizePower aggregateRollup

-- | Finalize grouping power set as cube power set.
cube   :: AggregatingPowerSet a -> (a, AggregateElem)
cube   =  finalizePower aggregateCube

-- | Finalize grouping set list.
groupingSets :: Monad m => AggregatingSetList a -> (a, AggregateElem)
groupingSets =  second aggregateSets . extractTermList

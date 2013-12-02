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

  AggregatingSet, AggregatingSetList, AggregatingPowerSet,

  unsafeAggregateWithTerms,

  -- * Result
  extractAggregateTerms
  ) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State (StateT, runStateT, modify)
import Control.Applicative (Applicative, (<$>))
import Control.Arrow (second, (>>>))

import Database.Relational.Query.Context (Flat, Aggregated, Set, Power, SetList)
import Database.Relational.Query.Component
  (AggregateElem, aggregateColumnRef, AggregateSet, AggregateKey, aggregatePowerKey)
import Database.Relational.Query.Monad.Trans.ListState
  (TermsContext, primeTermsContext, appendTerm, termsList)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection

import Database.Relational.Query.Monad.Class
  (MonadRestrict(..), MonadQuery(..), MonadAggregate(..), MonadAggregateKey(..))


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
type AggregatingSet      = Aggregatings Set     AggregateElem

-- | Context type building grouping sets list.
type AggregatingSetList  = Aggregatings SetList AggregateSet

-- | Context type building power group set.
type AggregatingPowerSet = Aggregatings Power   AggregateKey

-- | Aggregated 'MonadRestrict'.
instance MonadRestrict c m => MonadRestrict c (AggregatingSet m) where
  restrictContext =  aggregatings . restrictContext

-- | Aggregated 'MonadQuery'.
instance MonadQuery m => MonadQuery (AggregatingSet m) where
  restrictJoin  =  aggregatings . restrictJoin
  unsafeSubQuery na = aggregatings . unsafeSubQuery na

-- | Unsafely update aggregating context.
updateAggregatingContext :: Monad m => (TermsContext at -> TermsContext at) -> Aggregatings ac at m ()
updateAggregatingContext =  Aggregatings . modify

-- | Unsafely add not-typeful aggregating terms.
unsafeAggregateWithTerms :: Monad m => [at] -> Aggregatings ac at m ()
unsafeAggregateWithTerms gbs = updateAggregatingContext . foldr (>>>) id $ map appendTerm gbs

-- | Add aggregating terms using projection.
aggregateWithProjection :: Monad m
                    => (Projection Flat r -> [at])                    -- ^ Get aggregating terms from Projection
                    -> Projection Flat r                              -- ^ Group-by term to add
                    -> Aggregatings ac at m (Projection Aggregated r) -- ^ Result aggregated context
aggregateWithProjection terms p = do
  unsafeAggregateWithTerms . terms $ p
  return $ Projection.unsafeToAggregated p

-- | Aggregated query instance.
instance MonadQuery m => MonadAggregate (AggregatingSet m) where
  aggregateKey' = aggregateWithProjection $ map aggregateColumnRef . Projection.columns

-- | Aggregate key specify instance
instance (Functor m, Monad m) => MonadAggregateKey (AggregatingPowerSet m) where
  aggregateKey = fmap Projection.just . aggregateWithProjection ((:[]) . aggregatePowerKey . Projection.columns)

-- | Run 'Aggregatings' to get terms list.
extractAggregateTerms :: (Monad m, Functor m) => Aggregatings ac at m a -> m (a, [at])
extractAggregateTerms q = second termsList <$> runAggregatingPrime q

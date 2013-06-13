{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.Relational.Query.Aggregation
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines aggregated query projection type structure and interfaces.
module Database.Relational.Query.Aggregation (
  -- * Projection definition for Aggregated query
  Aggregation,
  projection,
  mapAggregation,

  unsafeFromProjection,

  -- * Aggregated Query Projections
  compose,

  pi, piMaybe, piMaybe',

  flattenMaybe, just
  ) where


import Prelude hiding (pi)

import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Pi (Pi)


-- | Projection for aggregated query.
newtype Aggregation r = Aggregation (Projection r)

-- | Get projection of normal query.
projection :: Aggregation r -> Projection r
projection (Aggregation p) = p

-- | Map from 'Projection' into 'Aggregation'.
mapAggregation ::  (Projection a -> Projection b) -> Aggregation a -> Aggregation b
mapAggregation f = Aggregation . f . projection

-- | Unsafely make 'Aggregation' from 'Projection'.
unsafeFromProjection :: Projection r -> Aggregation r
unsafeFromProjection =  Aggregation

-- | Concatenate 'Aggregation'.
compose :: Aggregation a -> Aggregation b -> Aggregation (c a b)
compose (Aggregation a) (Aggregation b) = Aggregation $ a `Projection.compose` b

-- | Map Projection path into Aggregation.
definePi :: (Projection a -> Pi a' b' -> Projection b) -> Aggregation a -> Pi a' b' -> Aggregation b
definePi (!!!) p pi' = mapAggregation (!!! pi') p

-- | Trace projection path to get smaller 'Aggregation'.
pi :: Aggregation a -- ^ Source projection. 'Maybe' type
   -> Pi a b        -- ^ Projection path
   -> Aggregation b -- ^ Narrower projection
pi =  definePi Projection.pi

-- | Trace projection path to get smaller 'Aggregation'. From 'Maybe' type to 'Maybe' type.
piMaybe :: Aggregation (Maybe a) -- ^ Source projection. 'Maybe' type
        -> Pi a b                -- ^ Projection path
        -> Aggregation (Maybe b) -- ^ Narrower projection. 'Maybe' type result
piMaybe = definePi Projection.piMaybe

-- | Trace projection path to get smaller 'Aggregation'. From 'Maybe' type to 'Maybe' type.
--   Projection path's leaf is 'Maybe' case.
piMaybe' :: Aggregation (Maybe a) -- ^ Source projection. 'Maybe' type
         -> Pi a (Maybe b)        -- ^ Projection path. 'Maybe' type leaf
         -> Aggregation (Maybe b) -- ^ Narrower projection. 'Maybe' type result
piMaybe' =  definePi Projection.piMaybe'

-- | Composite nested 'Maybe' on projection phantom type.
flattenMaybe :: Aggregation (Maybe (Maybe a)) -> Aggregation (Maybe a)
flattenMaybe =  mapAggregation Projection.flattenMaybe

-- | Cast into 'Maybe' on projection phantom type.
just :: Aggregation a -> Aggregation (Maybe a)
just =  mapAggregation Projection.just

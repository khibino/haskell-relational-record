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
  Aggregation, projection,
  mapAggregation,

  compose,

  just, flattenMaybe,

  pi, piMaybe, piMaybe',

  unsafeFromProjection
  ) where


import Prelude hiding (pi)

import Database.Record (PersistableWidth)

import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Pi (Pi)


-- | Projection for aggregated query.
newtype Aggregation r = Aggregation (Projection r)

-- | Get projection of normal query.
projection :: Aggregation r -> Projection r
projection (Aggregation p) = p

mapAggregation ::  (Projection a -> Projection b) -> Aggregation a -> Aggregation b
mapAggregation f = Aggregation . f . projection

unsafeFromProjection :: Projection r -> Aggregation r
unsafeFromProjection =  Aggregation

compose :: Aggregation a -> Aggregation b -> Aggregation (c a b)
compose (Aggregation a) (Aggregation b) = Aggregation $ a `Projection.compose` b

just :: Aggregation a -> Aggregation (Maybe a)
just =  mapAggregation Projection.just

flattenMaybe :: Aggregation (Maybe (Maybe a)) -> Aggregation (Maybe a)
flattenMaybe =  mapAggregation Projection.flattenMaybe

definePi :: (Projection a -> Pi a' b' -> Projection b) -> Aggregation a -> Pi a' b' -> Aggregation b
definePi (!!!) p pi' = mapAggregation (!!! pi') p

pi :: PersistableWidth b => Aggregation a -> Pi a b -> Aggregation b
pi =  definePi Projection.pi

piMaybe :: PersistableWidth b => Aggregation (Maybe a) -> Pi a b -> Aggregation (Maybe b)
piMaybe = definePi Projection.piMaybe

piMaybe' :: PersistableWidth b => Aggregation (Maybe a) -> Pi a (Maybe b) -> Aggregation (Maybe b)
piMaybe' =  definePi Projection.piMaybe'

{-# LANGUAGE FlexibleContexts #-}

module Database.Relational.Query.Aggregation (
  Aggregation, projection,
  liftAggregation,

  compose,

  just, flattenMaybe,

  pi, piMaybe,

  unsafeFromProjection
  ) where

import Prelude hiding (pi)

import Database.Record (PersistableWidth)

import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Pi (Pi)


newtype Aggregation r = Aggregation (Projection r)

projection :: Aggregation r -> Projection r
projection (Aggregation p) = p

liftAggregation ::  (Projection a -> Projection b) -> Aggregation a -> Aggregation b
liftAggregation f = Aggregation . f . projection

unsafeFromProjection :: Projection r -> Aggregation r
unsafeFromProjection =  Aggregation

compose :: Aggregation a -> Aggregation b -> Aggregation (c a b)
compose (Aggregation a) (Aggregation b) = Aggregation $ a `Projection.compose` b

just :: Aggregation a -> Aggregation (Maybe a)
just =  liftAggregation Projection.just

flattenMaybe :: Aggregation (Maybe (Maybe a)) -> Aggregation (Maybe a)
flattenMaybe =  liftAggregation Projection.flattenMaybe

definePi :: (Projection a -> Pi a' b' -> Projection b) -> Aggregation a -> Pi a' b' -> Aggregation b
definePi (!!!) p pi' = liftAggregation (!!! pi') p

pi :: PersistableWidth b => Aggregation a -> Pi a b -> Aggregation b
pi =  definePi Projection.pi

piMaybe :: PersistableWidth b => Aggregation (Maybe a) -> Pi a b -> Aggregation (Maybe b)
piMaybe = definePi Projection.piMaybe

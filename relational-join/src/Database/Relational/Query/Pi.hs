-- |
-- Module      : Database.Relational.Query.Pi
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines typed projection path objects.
-- Contains normal interfaces.
module Database.Relational.Query.Pi (
  -- * Projection path
  Pi((:*)),
  leafIndex,

  fst', snd',

  -- * Projection path unit
  PiUnit, offset,
  pairPiFstUnit, pairPiSndUnit,

  fst'', snd''
  ) where

import Database.Record
  (PersistableWidth, persistableWidth, PersistableRecordWidth)
import Database.Record.Persistable
  (runPersistableRecordWidth)

import Database.Relational.Query.Pi.Unsafe
  (PiUnit, offset, Pi ((:*), Leaf), definePiUnit)


-- | Get index of flat SQL value list from typed projection path.
leafIndex :: Pi r f -> Int
leafIndex =  rec  where
  rec :: Pi r f -> Int
  rec (Leaf pi0) = offset pi0
  rec (pi0 :* x) = offset pi0 + rec x

-- | Projection path unit like fst of tuple.
pairPiFstUnit :: PiUnit (c a b) a
pairPiFstUnit =  definePiUnit 0

-- | Devivation rule of projection path unit like snd of tuple.
pairPiSndUnit' :: PersistableRecordWidth a -> PiUnit (c a b) b
pairPiSndUnit' pw = definePiUnit (runPersistableRecordWidth pw)

-- | Devivated projection path unit like snd of tuple.
pairPiSndUnit :: PersistableWidth a => PiUnit (c a b) b
pairPiSndUnit =  pairPiSndUnit' persistableWidth


-- | Projection path unit for fst of tuple.
fst'' :: PiUnit (a, b) a
fst'' =  pairPiFstUnit

-- | Projection path unit for snd of tuple.
snd'' :: PersistableWidth a =>  PiUnit (a, b) b
snd'' =  pairPiSndUnit

-- | Projection path for fst of tuple.
fst' :: Pi (a, b) a
fst' =  Leaf fst''

-- | Projection path for snd of tuple.
snd' :: PersistableWidth a =>  Pi (a, b) b
snd' =  Leaf snd''

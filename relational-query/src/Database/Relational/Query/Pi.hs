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
  Pi, pfmap, pap, (<.>), (<?.>), (<?.?>),

  id', fst', snd'
  ) where

import Database.Record
  (PersistableWidth, persistableWidth, PersistableRecordWidth)
import Database.Record.Persistable
  (runPersistableRecordWidth)

import Database.Relational.Query.Pi.Unsafe
  (Pi, pfmap, pap, (<.>), (<?.>), (<?.?>), definePi)

-- | Identity projection path.
id' :: PersistableWidth a => Pi a a
id' =  definePi 0

-- | Projection path for fst of tuple.
fst' :: PersistableWidth a => Pi (a, b) a -- ^ Projection path of fst.
fst' =  definePi 0

snd'' :: PersistableWidth b => PersistableRecordWidth a -> Pi (a, b) b
snd'' wa = definePi (runPersistableRecordWidth wa)

-- | Projection path for snd of tuple.
snd' :: (PersistableWidth a, PersistableWidth b) =>  Pi (a, b) b -- ^ Projection path of snd.
snd' =  snd'' persistableWidth

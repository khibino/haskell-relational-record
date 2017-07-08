{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.Relational.Query.Pi.Unsafe
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines typed projection path objects.
-- Contains internal structure and unsafe interfaces.
module Database.Relational.Query.Pi.Unsafe (
  -- * Projection path
  Pi,

  pfmap, pap,

  width', width,

  (<.>), (<?.>), (<?.?>),

  pi,

  definePi, defineDirectPi', defineDirectPi,

  unsafeExpandIndexes
  ) where

import Prelude hiding (pi)
import Data.Array (listArray, (!))

import Database.Record.Persistable
  (PersistableRecordWidth, runPersistableRecordWidth, unsafePersistableRecordWidth, (<&>),
   PersistableWidth (persistableWidth), maybeWidth)

import Database.Relational.Query.ProjectableClass
  (ProductConstructor (..), ProjectableFunctor (..), ProjectableApplicative (..), )


-- | Projection path primary structure type.
data Pi' r0 r1 = Leftest Int
               | Map [Int]

unsafePiAppend' :: Pi' a b' -> Pi' b c' -> Pi' a c
unsafePiAppend' = d  where
  d (Leftest i) (Leftest j) = Leftest $ i + j
  d (Leftest i) (Map js)    = Map $ map (i +) js
  d (Map is)    (Leftest j) = Map $ drop j is
  d (Map is)    (Map js)    = Map   [ is' ! j | j <- js ]  where
    is' = listArray (0, length is) is

-- | Projection path from type 'r0' into type 'r1'.
--   This type also indicate key object which type is 'r1' for record type 'r0'.
data Pi r0 r1 = Pi (Pi' r0 r1) (PersistableRecordWidth r1)

unsafePiAppend :: (PersistableRecordWidth c' -> PersistableRecordWidth c)
                  -> Pi a b' -> Pi b c' -> Pi a c
unsafePiAppend f (Pi p0 _) (Pi p1 w) =
  Pi (p0 `unsafePiAppend'` p1) (f w)

-- | Unsafely untype key to expand indexes.
unsafeExpandIndexes :: Pi a b -> [Int]
unsafeExpandIndexes = d  where
  d (Pi (Map is) _)    = is
  d (Pi (Leftest i) w) = [ i .. i + w' - 1 ]  where
    w' = runPersistableRecordWidth w

-- | Unsafely cast width proof object of record. Result record must be same width.
unsafeCastRecordWidth :: PersistableRecordWidth a -> PersistableRecordWidth a'
unsafeCastRecordWidth =  unsafePersistableRecordWidth . runPersistableRecordWidth

unsafeCast :: Pi a b' -> Pi a b
unsafeCast =  c  where
  d (Leftest i) = Leftest i
  d (Map m)     = Map m
  c (Pi p w)    = Pi (d p) (unsafeCastRecordWidth w)

-- | Projectable fmap of 'Pi' type.
pfmap :: ProductConstructor (a -> b)
      => (a -> b) -> Pi r a -> Pi r b
_ `pfmap` p = unsafeCast p

-- | Projectable ap of 'Pi' type.
pap :: Pi r (a -> b) -> Pi r a -> Pi r b
pap b@(Pi _ wb) c@(Pi _ wc) =
   Pi
   (Map $ unsafeExpandIndexes b ++ unsafeExpandIndexes c)
   (unsafeCastRecordWidth $ wb <&> wc)

-- | Compose seed of projection path 'Pi' which has record result type.
instance ProjectableFunctor (Pi a) where
  (|$|) = pfmap

-- | Compose projection path 'Pi' which has record result type using applicative style.
instance ProjectableApplicative (Pi a) where
  (|*|) = pap

-- | Get record width proof object.
width' :: Pi r ct -> PersistableRecordWidth ct
width' (Pi _ w) = w

-- | Get record width.
width :: Pi r a -> Int
width =  runPersistableRecordWidth . width'

-- | Compose projection path.
(<.>) :: Pi a b -> Pi b c -> Pi a c
(<.>) = unsafePiAppend id

-- | Compose projection path. 'Maybe' phantom functor is 'map'-ed.
(<?.>) :: Pi a (Maybe b) -> Pi b c -> Pi a (Maybe c)
(<?.>) = unsafePiAppend maybeWidth

-- | Compose projection path. 'Maybe' phantom functors are 'join'-ed like '>=>'.
(<?.?>) :: Pi a (Maybe b) -> Pi b (Maybe c) -> Pi a (Maybe c)
(<?.?>) = unsafePiAppend id

infixl 8 <.>, <?.>, <?.?>

-- | Unsafely project untyped value list.
pi :: [a] -> Pi r0 r1 -> [a]
pi cs (Pi p' w) = d p'  where
  d (Leftest i) = take (runPersistableRecordWidth w) . drop i $ cs
  d (Map is)    = [cs' ! i | i <- is]
  cs' = listArray (0, length cs) cs

-- | Unsafely define projection path from type 'r0' into type 'r1'.
definePi' :: PersistableRecordWidth r1
          -> Int      -- ^ Index of flat SQL value list
          -> Pi r0 r1 -- ^ Result projection path
definePi' pw i = Pi (Leftest i) pw

-- | Unsafely define projection path from type 'r0' into type 'r1'.
--   Use inferred 'PersistableRecordWidth'.
definePi :: PersistableWidth r1
         => Int      -- ^ Index of flat SQL value list
         -> Pi r0 r1 -- ^ Result projection path
definePi = definePi' persistableWidth

-- | Unsafely define projection path from type 'r0' into type 'r1'.
defineDirectPi' :: PersistableRecordWidth r1
                -> [Int]    -- ^ Indexes of flat SQL value list
                -> Pi r0 r1 -- ^ Result projection path
defineDirectPi' pw is = Pi (Map is) pw

-- | Unsafely define projection path from type 'r0' into type 'r1'.
--   Use inferred 'PersistableRecordWidth'.
defineDirectPi :: PersistableWidth r1
               => [Int]    -- ^ Indexes of flat SQL value list
               -> Pi r0 r1 -- ^ Result projection path
defineDirectPi = defineDirectPi' persistableWidth

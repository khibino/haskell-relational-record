{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.Relational.Query.Pi.Unsafe
-- Copyright   : 2013 Kei Hibino
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

  pfmap,
  piZip, unsafeCastPi,

  width',

  (<.>), (<?.>), (<??.>),

  pi,

  definePi, defineDirectPi', defineDirectPi,

  unsafeExpandIndexes
  ) where

import Prelude hiding (pi)
import Data.Array (listArray, (!))

import Database.Record.Persistable
  (PersistableRecordWidth, runPersistableRecordWidth, unsafePersistableRecordWidth, (<&>),
   PersistableWidth (persistableWidth), maybeWidth)

import Database.Relational.Query.Pure (ProductConstructor (..))

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
  d (Pi (Leftest i) w) = [ i .. i + width - 1 ]  where
    width = runPersistableRecordWidth w

unsafeCast :: Pi a b' -> Pi a b
unsafeCast =  c  where
  d (Leftest i) = Leftest i
  d (Map m)     = Map m
  c (Pi p w)    = Pi (d p) (unsafePersistableRecordWidth . runPersistableRecordWidth $ w)

-- | Unsafely cast result type of Pi.
unsafeCastPi :: Pi a b' -> Pi a b
unsafeCastPi =  unsafeCast

-- | Projectable fmap of 'Pi' type.
pfmap :: ProductConstructor (a -> b)
      => (a -> b) -> Pi r a -> Pi r b
_ `pfmap` p = unsafeCast p

-- | Zipping two projection path.
piZip :: Pi a b -> Pi a c -> Pi a (b, c)
piZip b@(Pi _ wb) c@(Pi _ wc) =
   Pi
   (Map $ unsafeExpandIndexes b ++ unsafeExpandIndexes c)
   (wb <&> wc)

-- | Get record width proof object.
width' :: Pi r ct -> PersistableRecordWidth ct
width' (Pi _ w) = w

-- | Compose projection path.
(<.>) :: Pi a b -> Pi b c -> Pi a c
(<.>) = unsafePiAppend id

-- | Compose projection path.
(<?.>) :: Pi a (Maybe b) -> Pi b c -> Pi a (Maybe c)
(<?.>) = unsafePiAppend maybeWidth

-- | Compose projection path.
(<??.>) :: Pi a (Maybe b) -> Pi b (Maybe c) -> Pi a (Maybe c)
(<??.>) = unsafePiAppend id

infixl 8 <.>, <?.>, <??.>

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
--   Use infered 'PersistableRecordWidth'.
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
--   Use infered 'PersistableRecordWidth'.
defineDirectPi :: PersistableWidth r1
               => [Int]    -- ^ Indexes of flat SQL value list
               -> Pi r0 r1 -- ^ Result projection path
defineDirectPi = defineDirectPi' persistableWidth

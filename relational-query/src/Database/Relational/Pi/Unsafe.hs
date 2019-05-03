{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Pi.Unsafe
-- Copyright   : 2013-2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines typed projection path objects.
-- Contains internal structure and unsafe interfaces.
module Database.Relational.Pi.Unsafe (
  -- * Projection path
  Pi,

  width', width,

  (<.>), (<?.>), (<?.?>),

  pi,

  definePi, defineDirectPi', defineDirectPi,

  expandIndexes', expandIndexes,

  -- * Deprecated
  unsafeExpandIndexes',
  unsafeExpandIndexes
  ) where

import Prelude hiding (pi, (.), id)
import Control.Category (Category (..), (>>>))
import Data.Array (listArray, (!))
import Data.Functor.ProductIsomorphic
  (ProductIsoFunctor, (|$|), ProductIsoApplicative, pureP, (|*|),
   ProductIsoEmpty, pureE, peRight, peLeft, )

import Database.Record.Persistable
  (PersistableRecordWidth, runPersistableRecordWidth, unsafePersistableRecordWidth,
   PersistableWidth (persistableWidth), maybeWidth)


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
newtype Pi r0 r1 = Pi { runPi :: PersistableRecordWidth r0 -> (Pi' r0 r1, PersistableRecordWidth r1) }

instance PersistableWidth r0 => Show (Pi r0 r1) where
  show p = unwords ["Pi", show $ unsafeExpandIndexes p]

unsafePiAppend :: (PersistableRecordWidth b' -> PersistableRecordWidth b)
               -> (PersistableRecordWidth c' -> PersistableRecordWidth c)
               -> Pi a b' -> Pi b c' -> Pi a c
unsafePiAppend wbf wcf (Pi f) (Pi g) = Pi $ \wa ->
  let (pab, wb) = f wa
      (pbc, wc) = g $ wbf wb
  in (pab `unsafePiAppend'` pbc, wcf wc)

-- | Expand indexes from key.
expandIndexes' :: PersistableRecordWidth a -> Pi a b -> [Int]
expandIndexes' wa (Pi f) = d $ f wa where
  d (Map is, _)    = is
  d (Leftest i, w) = [ i .. i + w' - 1 ]  where
    w' = runPersistableRecordWidth w

unsafeExpandIndexes' :: PersistableRecordWidth a -> Pi a b -> [Int]
unsafeExpandIndexes' = expandIndexes'
{-# DEPRECATED unsafeExpandIndexes' "Use expandIndexes' instead of this." #-}

-- | Expand indexes from key. Infered width version.
expandIndexes :: PersistableWidth a => Pi a b -> [Int]
expandIndexes = expandIndexes' persistableWidth

unsafeExpandIndexes :: PersistableWidth a => Pi a b -> [Int]
unsafeExpandIndexes = expandIndexes
{-# DEPRECATED unsafeExpandIndexes "use expandIndexes instead of this." #-}

-- | Unsafely cast width proof object of record. Result record must be same width.
unsafeCastRecordWidth :: PersistableRecordWidth a -> PersistableRecordWidth a'
unsafeCastRecordWidth =  unsafePersistableRecordWidth . runPersistableRecordWidth

unsafeCast :: Pi a b' -> Pi a b
unsafeCast =  c  where
  d (Leftest i) = Leftest i
  d (Map m)     = Map m
  c (Pi f)    = Pi $ \wa ->
    let (pb, wb) = f wa in
    (d pb, unsafeCastRecordWidth wb)

-- | 'Pi' with zero width which projects to unit
pzero :: Pi a ()
pzero = Pi $ \_ -> (Map [], persistableWidth)

-- | Map projection path 'Pi' which has record result type.
instance ProductIsoFunctor (Pi a) where
  _ |$| p = unsafeCast p

-- | Compose projection path 'Pi' which has record result type using applicative style.
instance ProductIsoApplicative (Pi a) where
  pureP _ = unsafeCast pzero
  pab |*| pb =
    Pi $ \wr ->
           let (_, wab) = runPi pab wr
               (_, wb)  = runPi pb  wr in
             (Map $ unsafeExpandIndexes' wr pab ++ unsafeExpandIndexes' wr pb,
              wab |*| wb)

instance ProductIsoEmpty (Pi a) () where
  pureE   = pzero
  peRight = unsafeCast
  peLeft  = unsafeCast

-- | Get record width proof object.
width' :: PersistableWidth r => Pi r ct -> PersistableRecordWidth ct
width' (Pi f) = snd $ f persistableWidth

-- | Get record width.
width :: PersistableWidth r => Pi r a -> Int
width =  runPersistableRecordWidth . width'

justWidth :: PersistableRecordWidth (Maybe a) -> PersistableRecordWidth a
justWidth = unsafeCastRecordWidth


instance Category Pi where
  id = Pi $ \pw -> (Leftest 0, pw)
  Pi fb . Pi fa = Pi $ \wa ->
    let (pb, wb) = fa wa
        (pc, wc) = fb wb
    in (unsafePiAppend' pb pc, wc)

-- | Compose projection path.
(<.>) :: Pi a b -> Pi b c -> Pi a c
(<.>) = (>>>)

-- | Compose projection path. 'Maybe' phantom functor is 'map'-ed.
(<?.>) :: Pi a (Maybe b) -> Pi b c -> Pi a (Maybe c)
(<?.>) = unsafePiAppend justWidth maybeWidth

-- | Compose projection path. 'Maybe' phantom functors are 'join'-ed like '>=>'.
(<?.?>) :: Pi a (Maybe b) -> Pi b (Maybe c) -> Pi a (Maybe c)
(<?.?>) = unsafePiAppend justWidth id

infixl 8 <.>, <?.>, <?.?>

-- | Unsafely project untyped value list.
pi :: PersistableRecordWidth r0 -> Pi r0 r1 -> [a] -> [a]
pi w0 (Pi f) cs = d p'  where
  (p', w1) = f w0
  d (Leftest i) = take (runPersistableRecordWidth w1) . drop i $ cs
  d (Map is)    = [cs' ! i | i <- is]
  cs' = listArray (0, length cs) cs

-- | Unsafely define projection path from type 'r0' into type 'r1'.
definePi' :: PersistableRecordWidth r1
          -> Int      -- ^ Index of flat SQL value list
          -> Pi r0 r1 -- ^ Result projection path
definePi' pw i = Pi $ \_ -> (Leftest i, pw)

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
defineDirectPi' pw is = Pi $ \_ -> (Map is, pw)

-- | Unsafely define projection path from type 'r0' into type 'r1'.
--   Use inferred 'PersistableRecordWidth'.
defineDirectPi :: PersistableWidth r1
               => [Int]    -- ^ Indexes of flat SQL value list
               -> Pi r0 r1 -- ^ Result projection path
defineDirectPi = defineDirectPi' persistableWidth

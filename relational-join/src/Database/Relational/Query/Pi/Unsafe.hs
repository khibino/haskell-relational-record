{-# LANGUAGE ExistentialQuantification #-}

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
  -- * Projection path unit
  PiUnit, offset, definePiUnit,

  -- * Projection path
  Pi ((:*), Leaf),

  defineColumn
  ) where

-- | Projection path unit from record type 'r' into column type 'ct'.
newtype PiUnit r ct = PiUnit Int
-- data PiUnit r ct = PiUnit
--                { offset  :: Int
--                , column  :: Column r ct
--                }

-- | Get index of flat SQL value list from typed projection path unit.
offset :: PiUnit r ct -> Int
offset (PiUnit off) = off

-- | Projection path from record type 'r' into column type 'ct'.
data Pi r ct = forall r' . PiUnit r r' :* Pi r' ct
             |             Leaf (PiUnit r ct)

infixr 9 :*


-- | Unsafely define projection path from record type 'r' into column type 'ct'.
defineColumn :: Int     -- ^ Index of flat SQL value list
             -> Pi r ct -- ^ Result projection path
defineColumn =  Leaf . PiUnit

-- | Unsafely define projection path unit from record type 'r' into column type 'ct'.
definePiUnit :: Int         -- ^ Index of flat SQL value list
             -> PiUnit r ct -- ^ Result projection path
definePiUnit =  PiUnit

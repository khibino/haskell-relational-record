{-# LANGUAGE ExistentialQuantification #-}

module Database.Relational.Query.Pi.Unsafe (
  PiUnit, offset, definePiUnit,

  Pi ((:*), Leaf),

  defineColumn
  ) where

newtype PiUnit r ft = PiUnit Int
-- data PiUnit r ft = PiUnit
--                { offset  :: Int
--                , column  :: Column r ft
--                }

offset :: PiUnit r ft -> Int
offset (PiUnit off) = off

data Pi r ft = forall r' . PiUnit r r' :* Pi r' ft
             |             Leaf (PiUnit r ft)

infixr 9 :*


defineColumn :: Int -> Pi r ft
defineColumn =  Leaf . PiUnit

definePiUnit :: Int -> PiUnit r ft
definePiUnit =  PiUnit

{-# LANGUAGE ExistentialQuantification #-}

module Database.Relational.Query.Pi.Unsafe (
  Column, index,

  PiUnit (offset), definePiUnit,

  Pi ((:*), Leaf),

  defineColumn
  ) where

newtype PiUnit r ft = PiUnit { offset :: Int }
-- data PiUnit r ft = PiUnit
--                { offset  :: Int
--                , column  :: Column r ft
--                }

-- newtype Column r ft = Column { index :: Int }
type Column = PiUnit

index :: Column r ft -> Int
index =  offset

data Pi r ft = forall r' . PiUnit r r' :* Pi r' ft
             |             Leaf (Column r ft)

infixr 9 :*


defineColumn :: Int -> Pi r ft
defineColumn =  Leaf . PiUnit

-- definePi :: Int -> Column r ft -> Pi r ft
definePiUnit :: Int -> PiUnit r ft
definePiUnit =  PiUnit

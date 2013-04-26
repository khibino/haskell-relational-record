{-# LANGUAGE ExistentialQuantification #-}

module Database.Relational.Query.Pi.Unsafe (
  Column (index),

  Pi (offset), definePi,

  PiSeq ((:*), Leaf),

  defineColumn
  ) where

newtype Column r ft = Column { index :: Int }

newtype Pi r ft = Pi { offset :: Int }
-- data Pi r ft = Pi
--                { offset  :: Int
--                , column  :: Column r ft
--                }

data PiSeq r ft = forall r' . Pi r r' :* PiSeq r' ft
                |             Leaf (Column r ft)

infixr 9 :*


defineColumn :: Int -> PiSeq r ft
defineColumn =  Leaf . Column

-- definePi :: Int -> Column r ft -> Pi r ft
definePi :: Int -> Pi r ft
definePi =  Pi

module Database.Relational.Query.Pi (
  PiUnit,
  pairPiFstUnit, pairPiSndUnit, fst', snd',

  Pi((:*)),
  leafIndex,
  ) where

import Database.Relational.Query.Pi.Unsafe
  (index, PiUnit, offset, Pi ((:*), Leaf), definePiUnit)


leafIndex :: Pi r f -> Int
leafIndex =  rec  where
  rec :: Pi r f -> Int
  rec (Leaf pi0) = index pi0
  rec (pi0 :* x) = offset pi0 + rec x

pairPiFstUnit :: PiUnit (c a b) a
pairPiFstUnit =  definePiUnit 0 -- (Column 0)

pairPiSndUnit :: Int -> PiUnit (c a b) b
pairPiSndUnit off = definePiUnit off -- (Column 1)

fst' :: PiUnit (a, b) a
fst' =  pairPiFstUnit

snd' :: Int -> PiUnit (a, b) b
snd' =  pairPiSndUnit

module Database.Relational.Query.Pi (
  Column,

  Pi,
  pairPiFst, pairPiSnd, fst', snd',

  PiSeq((:*)),
  leafIndex,
  ) where

import Database.Relational.Query.Pi.Unsafe
  (Column, index, Pi, offset, PiSeq ((:*), Leaf), definePi)


leafIndex :: PiSeq r f -> Int
leafIndex =  rec  where
  rec :: PiSeq r f -> Int
  rec (Leaf pi0) = index pi0
  rec (pi0 :* x) = offset pi0 + rec x

pairPiFst :: Pi (c a b) a
pairPiFst =  definePi 0 -- (Column 0)

pairPiSnd :: Int -> Pi (c a b) b
pairPiSnd off = definePi off -- (Column 1)

fst' :: Pi (a, b) a
fst' =  pairPiFst

snd' :: Int -> Pi (a, b) b
snd' =  pairPiSnd

module Database.Relational.Query.Pi (
  PiUnit,
  pairPiFstUnit, pairPiSndUnit,

  fst'', snd'',
  fst', snd',

  Pi((:*)),
  leafIndex,
  ) where

import Database.Record.Persistable
  (PersistableWidth, persistableWidth,
   PersistableRecordWidth, runPersistableRecordWidth)

import Database.Relational.Query.Pi.Unsafe
  (PiUnit, offset, Pi ((:*), Leaf), definePiUnit)


leafIndex :: Pi r f -> Int
leafIndex =  rec  where
  rec :: Pi r f -> Int
  rec (Leaf pi0) = offset pi0
  rec (pi0 :* x) = offset pi0 + rec x

pairPiFstUnit :: PiUnit (c a b) a
pairPiFstUnit =  definePiUnit 0

pairPiSndUnit' :: PersistableRecordWidth a -> PiUnit (c a b) b
pairPiSndUnit' pw = definePiUnit (runPersistableRecordWidth pw)

pairPiSndUnit :: PersistableWidth a => PiUnit (c a b) b
pairPiSndUnit =  pairPiSndUnit' persistableWidth


fst'' :: PiUnit (a, b) a
fst'' =  pairPiFstUnit

snd'' :: PersistableWidth a =>  PiUnit (a, b) b
snd'' =  pairPiSndUnit

fst' :: Pi (a, b) a
fst' =  Leaf fst''

snd' :: PersistableWidth a =>  Pi (a, b) b
snd' =  Leaf snd''

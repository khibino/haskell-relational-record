{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Record.FromSql
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines interfaces
-- from list of SQL type into Haskell type.
module Database.Record.FromSql (
  -- * Conversion from list of SQL type into record type
  -- $recordFromSql
  RecordFromSql, runTakeRecord, runToRecord,
  createRecordFromSql,

  recordDeSerializer,

  (<&>),
  maybeRecord,

  -- * Inference rules of 'RecordFromSql' conversion
  FromSql (recordFromSql), recordFromSql',
  takeRecord, toRecord
  ) where

import Database.Record.Persistable
  (PersistableRecord,
   Persistable(persistable), PersistableType)
import qualified Database.Record.Persistable as Persistable
import Database.Record.KeyConstraint
  (HasColumnConstraint(keyConstraint), ColumnConstraint, NotNull, index)

import Control.Monad (liftM, ap)
import Control.Applicative ((<$>), Applicative(pure, (<*>)))

{- $recordFromSql
Structure of 'RecordFromSql' 'q' 'a' is similar to parser.
While running 'RecordFromSql' behavior is the same as parser 
which parse list of SQL type ['q'] stream.

So, 'RecordFromSql' 'q' is 'Monad' and 'Applicative' instance like parser monad.
When, you have data constructor and objects like below.

@
  data MyRecord = MyRecord Foo Bar Baz
@

@
  foo :: 'RecordFromSql' SqlValue Foo
  foo =  ...
  bar :: 'RecordFromSql' SqlValue Bar
  bar =  ...
  baz :: 'RecordFromSql' SqlValue Bar
  baz =  ...
@

You can get composed 'RecordFromSql' like below.

@
  myRecord :: RecordFromSql SqlValue MyRecord
  myRecord =  MyRecord \<$\> foo \<*\> bar \<*\> baz
@
-}

-- | Proof object type to convert from sql value type 'q' list into Haskell type 'a'.
newtype RecordFromSql q a = RecordFromSql ([q] -> (a, [q]))

-- | Run 'RecordFromSql' proof object.
--   Convert from list of SQL type ['q'] into Haskell type 'a' and rest of list ['q'].
runTakeRecord :: RecordFromSql q a -- ^ Proof object which has capability to convert
              -> [q]               -- ^ list of SQL type
              -> (a, [q])          -- ^ Haskell type and rest of list
runTakeRecord (RecordFromSql f) = f

-- | Axiom of 'RecordFromSql' for SQL type 'q' and Haskell type 'a'
createRecordFromSql :: ([q] -> (a, [q])) -- ^ Convert function body
                    -> RecordFromSql q a -- ^ Result proof object
createRecordFromSql =  RecordFromSql

-- | Derive 'RecordFromSql' proof object from 'PersistableRecord'.
recordDeSerializer :: PersistableRecord q a -> RecordFromSql q a
recordDeSerializer =  createRecordFromSql . Persistable.takeRecord

-- | Inferred 'RecordFromSql' proof object.
recordFromSql' :: Persistable q a => RecordFromSql q a
recordFromSql' =  recordDeSerializer persistable

-- | Run 'RecordFromSql' proof object. Convert from list of SQL type ['q'] into  Haskell type 'a'.
runToRecord :: RecordFromSql q a -- ^ Proof object which has capability to convert
            -> [q]               -- ^ list of SQL type
            -> a                 -- ^ Haskell type
runToRecord r = fst . runTakeRecord r

-- | 'Monad' instance like parser 'Monad'.
instance Monad (RecordFromSql q) where
  return a = createRecordFromSql ((,) a)
  ma >>= fmb =
    createRecordFromSql
    (\vals -> let (a, vals') = runTakeRecord ma vals
              in  runTakeRecord (fmb a) vals')

-- | Derived 'Functor' instance from 'Monad' instance
instance Functor (RecordFromSql q) where
  fmap = liftM

-- | Derived 'Applicative' instance from 'Monad' instance
instance Applicative (RecordFromSql q) where
  pure  = return
  (<*>) = ap

-- | Derivation rule of 'RecordFromSql' proof object for Haskell tuple (,) type.
(<&>) :: RecordFromSql q a -> RecordFromSql q b -> RecordFromSql q (a, b)
a <&> b = (,) <$> a <*> b

infixl 4 <&>


-- | Derivation rule of 'RecordFromSql' proof object for Haskell 'Maybe' type.
maybeRecord :: PersistableType q
            => RecordFromSql q a
            -> ColumnConstraint NotNull a
            -> RecordFromSql q (Maybe a)
maybeRecord rec pkey = createRecordFromSql mayToRec where
  mayToRec vals
    | vals !! index pkey /= Persistable.sqlNullValue = (Just a,  vals')
    | otherwise                         = (Nothing, vals')  where
      (a, vals') = runTakeRecord rec vals


-- | Inference rule interface for 'RecordFromSql' proof object.
class FromSql q a where
  -- | 'RecordFromSql' proof object.
  recordFromSql :: RecordFromSql q a

-- | Inference rule of 'RecordFromSql' proof object which can convert
--   from list of SQL type ['q'] into Haskell tuple ('a', 'b') type.
instance (FromSql q a, FromSql q b) => FromSql q (a, b)  where
  recordFromSql = recordFromSql <&> recordFromSql

-- | Inference rule of 'RecordFromSql' proof object which can convert
--   from list of SQL type ['q'] into Haskell 'Maybe' type.
instance (HasColumnConstraint NotNull a, FromSql q a, PersistableType q)
         => FromSql q (Maybe a)  where
  recordFromSql = maybeRecord recordFromSql keyConstraint

-- | Inference rule of 'RecordFromSql' proof object which can convert
--   from /empty/ list of SQL type ['q'] into Haskell unit () type.
instance FromSql q () where
  recordFromSql = recordFromSql'

-- | Run inferred 'RecordFromSql' proof object.
--   Convert from list of SQL type ['q'] into haskell type 'a' and rest of list ['q'].
takeRecord :: FromSql q a => [q] -> (a, [q])
takeRecord =  runTakeRecord recordFromSql

-- | Run inferred 'RecordFromSql' proof object.
--   Convert from list of SQL type ['q'] into haskell type 'a'.
toRecord :: FromSql q a => [q] -> a
toRecord =  runToRecord recordFromSql

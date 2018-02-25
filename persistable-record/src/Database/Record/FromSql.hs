{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}

-- |
-- Module      : Database.Record.FromSql
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines interfaces
-- from list of database value type into Haskell type.

module Database.Record.FromSql (
  -- * Conversion from list of database value type into record type
  RecordFromSql, runTakeRecord, runToRecord,
  createRecordFromSql,

  (<&>),
  maybeRecord,

  -- * Derivation rules of 'RecordFromSql' conversion
  FromSql (recordFromSql),
  takeRecord, toRecord,

  valueRecordFromSql,
  ) where

import GHC.Generics (Generic, Rep, U1 (..), K1 (..), M1 (..), (:*:)(..), to)
import Control.Applicative ((<$>), Applicative (pure, (<*>)))
import Control.Monad (liftM, ap)

import Database.Record.Persistable (PersistableType)
import qualified Database.Record.Persistable as Persistable
import Database.Record.KeyConstraint
  (HasColumnConstraint(columnConstraint), ColumnConstraint, NotNull, index)

{- |
'RecordFromSql' 'q' 'a' is data-type wrapping function
to convert from list of database value type (to receive from database) ['q'] into Haskell type 'a'

This structure is similar to parser.
While running 'RecordFromSql' behavior is the same as non-fail-able parser
which parse list of database value type ['q'] stream.

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
  baz :: 'RecordFromSql' SqlValue Baz
  baz =  ...
@

You can get composed 'RecordFromSql' like below.

@
  myRecord :: RecordFromSql SqlValue MyRecord
  myRecord =  MyRecord \<$\> foo \<*\> bar \<*\> baz
@
-}
newtype RecordFromSql q a = RecordFromSql ([q] -> (a, [q]))

-- | Run 'RecordFromSql' parser function object.
--   Convert from list of database value type ['q'] into Haskell type 'a' and rest of list ['q'].
runTakeRecord :: RecordFromSql q a -- ^ parser function object which has capability to convert
              -> [q]               -- ^ list of database value type
              -> (a, [q])          -- ^ Haskell type and rest of list
runTakeRecord (RecordFromSql f) = f

-- | Axiom of 'RecordFromSql' for database value type 'q' and Haskell type 'a'
createRecordFromSql :: ([q] -> (a, [q])) -- ^ Convert function body
                    -> RecordFromSql q a -- ^ Result parser function object
createRecordFromSql =  RecordFromSql

-- | Run 'RecordFromSql' parser function object. Convert from list of database value type ['q'] into  Haskell type 'a'.
runToRecord :: RecordFromSql q a -- ^ parser function object which has capability to convert
            -> [q]               -- ^ list of database value type
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

-- | Derivation rule of 'RecordFromSql' parser function object for Haskell tuple (,) type.
(<&>) :: RecordFromSql q a -> RecordFromSql q b -> RecordFromSql q (a, b)
a <&> b = (,) <$> a <*> b

infixl 4 <&>


-- | Derivation rule of 'RecordFromSql' parser function object for Haskell 'Maybe' type.
maybeRecord :: PersistableType q
            => RecordFromSql q a
            -> ColumnConstraint NotNull a
            -> RecordFromSql q (Maybe a)
maybeRecord rec pkey = createRecordFromSql mayToRec where
  mayToRec vals
    | vals !! index pkey /= Persistable.sqlNullValue = (Just a,  vals')
    | otherwise                         = (Nothing, vals')  where
      (a, vals') = runTakeRecord rec vals

{- |
'FromSql' 'q' 'a' is implicit rule to derive 'RecordFromSql' 'q' 'a' record parser function against type 'a'.

Generic programming (<https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#generic-programming>)
with default signature is available for 'FromSql' class,
so you can make instance like below:

@
  \{\-\# LANGUAGE DeriveGeneric \#\-\}
  import GHC.Generics (Generic)
  import Database.HDBC (SqlValue)
  --
  data Foo = Foo { ... } deriving Generic
  instance FromSql SqlValue Foo
@

-}
class FromSql q a where
  -- | 'RecordFromSql' 'q' 'a' record parser function.
  recordFromSql :: RecordFromSql q a

  default recordFromSql :: (Generic a, GFromSql q (Rep a)) => RecordFromSql q a
  recordFromSql = to <$> gFromSql


class GFromSql q f where
  gFromSql :: RecordFromSql q (f a)

instance GFromSql q U1 where
  gFromSql = createRecordFromSql $ (,) U1

instance (GFromSql q a, GFromSql q b) => GFromSql q (a :*: b) where
  gFromSql = (:*:) <$> gFromSql <*> gFromSql

instance GFromSql q a => GFromSql q (M1 i c a) where
  gFromSql = M1 <$> gFromSql

instance FromSql q a => GFromSql q (K1 i a) where
  gFromSql = K1 <$> recordFromSql


-- | Implicit derivation rule of 'RecordFromSql' parser function object which can convert
--   from list of database value type ['q'] into Haskell 'Maybe' type.
instance (HasColumnConstraint NotNull a, FromSql q a, PersistableType q)
         => FromSql q (Maybe a)  where
  recordFromSql = maybeRecord recordFromSql columnConstraint

-- | Implicit derivation rule of 'RecordFromSql' parser function object which can convert
--   from /empty/ list of database value type ['q'] into Haskell unit () type.
instance FromSql q ()  -- default generic instance

-- | Run implicit 'RecordFromSql' parser function object.
--   Convert from list of database value type ['q'] into haskell type 'a' and rest of list ['q'].
takeRecord :: FromSql q a => [q] -> (a, [q])
takeRecord =  runTakeRecord recordFromSql

-- | Run implicit 'RecordFromSql' parser function object.
--   Convert from list of database value type ['q'] into haskell type 'a'.
toRecord :: FromSql q a => [q] -> a
toRecord =  runToRecord recordFromSql

-- | Derivation rule of 'RecordFromSql' parser function object for value convert function.
valueRecordFromSql :: (q -> a) -> RecordFromSql q a
valueRecordFromSql d = createRecordFromSql $ \qs -> (d $ head qs, tail qs)

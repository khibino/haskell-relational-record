{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}

-- |
-- Module      : Database.Record.ToSql
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines interfaces
-- from Haskell type into list of database value type.
module Database.Record.ToSql (
  -- * Conversion from record type into list of database value type
  ToSqlM, RecordToSql, runFromRecord,
  createRecordToSql,

  (<&>),

  -- * Derivation rules of 'RecordToSql' conversion
  ToSql (recordToSql),
  putRecord, putEmpty, fromRecord, wrapToSql,

  valueRecordToSql,

  -- * Make parameter list for updating with key
  updateValuesByUnique',
  updateValuesByUnique,
  updateValuesByPrimary,

  untypedUpdateValuesIndex,
  unsafeUpdateValuesWithIndexes
  ) where

import GHC.Generics (Generic, Rep, U1 (..), K1 (..), M1 (..), (:*:)(..), from)
import Data.Array (listArray, (!))
import Data.Set (toList, fromList, (\\))
import Control.Monad.Trans.Writer (Writer, execWriter, tell)
import Data.DList (DList)
import qualified Data.DList as DList

import Database.Record.Persistable
  (PersistableSqlType, runPersistableNullValue, PersistableType (persistableType),
   PersistableRecordWidth, runPersistableRecordWidth, PersistableWidth(persistableWidth))
import Database.Record.KeyConstraint
  (Primary, Unique, KeyConstraint, HasKeyConstraint(keyConstraint), unique, indexes)


-- | Context type to convert into database value list.
type ToSqlM q a = Writer (DList q) a

runToSqlM :: ToSqlM q a -> [q]
runToSqlM =  DList.toList . execWriter

{- |
'RecordToSql' 'q' 'a' is data-type wrapping function
to convert from Haskell type 'a' into list of database value type (to send to database) ['q'].

This structure is similar to printer.
While running 'RecordToSql' behavior is the same as list printer.
which appends list of database value type ['q'] stream.
-}
newtype RecordToSql q a = RecordToSql (a -> ToSqlM q ())

runRecordToSql :: RecordToSql q a -> a -> ToSqlM q ()
runRecordToSql (RecordToSql f) = f

-- | Finalize 'RecordToSql' record printer.
wrapToSql :: (a -> ToSqlM q ()) -> RecordToSql q a
wrapToSql =  RecordToSql

-- | Run 'RecordToSql' printer function object. Convert from Haskell type 'a' into list of database value type ['q'].
runFromRecord :: RecordToSql q a -- ^ printer function object which has capability to convert
              -> a               -- ^ Haskell type
              -> [q]             -- ^ list of database value
runFromRecord r = runToSqlM . runRecordToSql r

-- | Axiom of 'RecordToSql' for database value type 'q' and Haksell type 'a'.
createRecordToSql :: (a -> [q])      -- ^ Convert function body
                  -> RecordToSql q a -- ^ Result printer function object
createRecordToSql f =  wrapToSql $ tell . DList.fromList . f

-- unsafely map record
mapToSql :: (a -> b) -> RecordToSql q b -> RecordToSql q a
mapToSql f x = wrapToSql $ runRecordToSql x . f

-- unsafely put product record
productToSql :: (c -> (a -> b -> ToSqlM q ()) -> ToSqlM q ())
             -> RecordToSql q a -> RecordToSql q b -> RecordToSql q c
productToSql run ra rb = wrapToSql $ \c -> run c $ \a b -> do
  runRecordToSql ra a
  runRecordToSql rb b

-- | Derivation rule of 'RecordToSql' printer function object for Haskell tuple (,) type.
(<&>) :: RecordToSql q a -> RecordToSql q b -> RecordToSql q (a, b)
(<&>) = productToSql $ flip uncurry

-- | Derivation rule of 'RecordToSql' printer function object for Haskell 'Maybe' type.
maybeRecord :: PersistableSqlType q -> PersistableRecordWidth a -> RecordToSql q a -> RecordToSql q (Maybe a)
maybeRecord qt w ra =  wrapToSql d  where
  d (Just r) = runRecordToSql ra r
  d Nothing  = tell $ DList.replicate (runPersistableRecordWidth w) (runPersistableNullValue qt)

infixl 4 <&>

{- |
'ToSql' 'q' 'a' is implicit rule to derive 'RecordToSql' 'q' 'a' record printer function for type 'a'.

So, 'ToSql' 'q' 'a' and 'RecordToSql' 'q 'a' are composable with monadic context.
When, you have data constructor and objects like below.

@
  data MyRecord = MyRecord Foo Bar Baz
@

@
  instance ToSql SqlValue Foo where
    ...
  instance ToSql SqlValue Bar where
    ...
  instance ToSql SqlValue Baz where
    ...
@

You can get composed 'ToSql' implicit rule like below.

@
  instance ToSql SqlValue MyRecord where
    recordToSql =
    recordToSql = wrapToSql $ \\ (MyRecord x y z) -> do
      putRecord x
      putRecord y
      putRecord z
@

-}
class ToSql q a where
  -- | Derived 'RecordToSql' printer function object.
  recordToSql :: RecordToSql q a

  default recordToSql :: (Generic a, GToSql q (Rep a)) => RecordToSql q a
  recordToSql = from `mapToSql` gToSql

class GToSql q f where
  gToSql :: RecordToSql q (f a)

instance GToSql q U1 where
  gToSql = wrapToSql $ \U1 -> tell DList.empty

instance (GToSql q a, GToSql q b) => GToSql q (a :*: b) where
  gToSql = productToSql (\ (a:*:b) f -> f a b) gToSql gToSql

instance GToSql q a => GToSql q (M1 i c a) where
  gToSql = (\(M1 a) -> a) `mapToSql` gToSql

instance ToSql q a => GToSql q (K1 i a) where
  gToSql = (\(K1 a) -> a) `mapToSql` recordToSql


-- | Implicit derivation rule of 'RecordToSql' printer function object which can convert
--   from Haskell 'Maybe' type into list of database value type ['q'].
instance (PersistableType q, PersistableWidth a, ToSql q a) => ToSql q (Maybe a)  where
  recordToSql = maybeRecord persistableType persistableWidth recordToSql

-- | Implicit derivation rule of 'RecordToSql' printer function object which can convert
--   from Haskell unit () type into /empty/ list of database value type ['q'].
instance ToSql q ()  -- default generic instance

-- | Run implicit 'RecordToSql' printer function object.
--   Context to convert haskell record type 'a' into lib of database value type ['q'].
putRecord :: ToSql q a => a -> ToSqlM q ()
putRecord =  runRecordToSql recordToSql

-- | Run 'RecordToSql' empty printer.
putEmpty :: () -> ToSqlM q ()
putEmpty =  putRecord

-- | Run implicit 'RecordToSql' printer function object.
--   Convert from haskell type 'a' into list of database value type ['q'].
fromRecord :: ToSql q a => a -> [q]
fromRecord =  runToSqlM . putRecord

-- | Derivation rule of 'RecordToSql' printer function object for value convert function.
valueRecordToSql :: (a -> q) -> RecordToSql q a
valueRecordToSql = createRecordToSql . ((:[]) .)

-- | Make untyped indexes to update column from key indexes and record width.
--   Expected by update form like
--
--  @
--   UPDATE /table/ SET /c0/ = ?, /c1/ = ?, /c2/ = ? ... WHERE /key0/ = ? AND /key1/ = ? AND key2 = ? ...
--  @
untypedUpdateValuesIndex :: [Int] -- ^ Key indexes
                         -> Int   -- ^ Record width
                         -> [Int] -- ^ Indexes to update other than key
untypedUpdateValuesIndex key width = otherThanKey  where
    maxIx = width - 1
    otherThanKey = toList $ fromList [0 .. maxIx] \\ fromList key

-- | Unsafely specify key indexes to convert from Haskell type `ra`
--   into database value `q` list expected by update form like
--
-- @
--   UPDATE /table/ SET /c0/ = ?, /c1/ = ?, /c2/ = ? ... WHERE /key0/ = ? AND /key1/ = ? AND /key2/ = ? ...
-- @
--
--   using 'RecordToSql' printer function object.
unsafeUpdateValuesWithIndexes :: RecordToSql q ra
                              -> [Int]
                              -> ra
                              -> [q]
unsafeUpdateValuesWithIndexes pr key a =
  [ valsA ! i | i <- otherThanKey ++ key ]  where
    vals = runFromRecord pr a
    width = length vals
    valsA = listArray (0, width - 1) vals
    otherThanKey = untypedUpdateValuesIndex key width

-- | Convert from Haskell type `ra` into database value `q` list expected by update form like
--
-- @
--   UPDATE /table/ SET /c0/ = ?, /c1/ = ?, /c2/ = ? ... WHERE /key0/ = ? AND /key1/ = ? AND /key2/ = ? ...
-- @
--
--   using 'RecordToSql' printer function object.
updateValuesByUnique' :: RecordToSql q ra
                      -> KeyConstraint Unique ra -- ^ Unique key table constraint printer function object.
                      -> ra
                      -> [q]
updateValuesByUnique' pr uk = unsafeUpdateValuesWithIndexes pr (indexes uk)

-- | Convert like 'updateValuesByUnique'' using implicit 'RecordToSql' printer function object.
updateValuesByUnique :: ToSql q ra
                     => KeyConstraint Unique ra -- ^ Unique key table constraint printer function object.
                     -> ra
                     -> [q]
updateValuesByUnique = updateValuesByUnique' recordToSql

-- | Convert like 'updateValuesByUnique'' using implicit 'RecordToSql' and 'ColumnConstraint'.
updateValuesByPrimary :: (HasKeyConstraint Primary ra, ToSql q ra)
                      => ra -> [q]
updateValuesByPrimary =  updateValuesByUnique (unique keyConstraint)

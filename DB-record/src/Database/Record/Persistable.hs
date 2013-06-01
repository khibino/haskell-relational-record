{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Record.Persistable
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines interfaces
-- between haskell type and SQL type value list.
module Database.Record.Persistable (
  -- * Specify SQL type
  PersistableSqlType, runPersistableNullValue, persistableSqlTypeFromNull,

  -- * Specify record width
  PersistableRecordWidth(runPersistableRecordWidth),
  valueWidth, (<&>), maybeWidth,

  -- * Bidirectional conversion between single field type and SQL type
  PersistableSqlValue, persistableSqlValue,
  toValue, fromValue,

  -- * Bidirectional conversion between record type and list of SQL type
  PersistableRecord, persistableRecord,
  toRecord, fromRecord, width, takeRecord,

  -- * Inference rules for proof objects
  persistableFromValue,

  PersistableType(..), sqlNullValue,
  PersistableValue (..), fromSql, toSql,
  derivedPersistableValueRecord,
  PersistableWidth (..), persistableRecordWidth,
  Persistable (..)
  ) where


-- | Proof object to specify type 'q' is SQL type
newtype PersistableSqlType q = PersistableSqlType q

-- | Null value of SQL type 'q'.
runPersistableNullValue :: PersistableSqlType q -> q
runPersistableNullValue (PersistableSqlType q) = q

-- | Unsafely generate 'PersistableSqlType' proof object from specified SQL null value which type is 'q'.
persistableSqlTypeFromNull :: q -> PersistableSqlType q
persistableSqlTypeFromNull =  PersistableSqlType


-- | Proof object to specify value type 'a' is convertible with SQL type 'q'
data PersistableSqlValue q a = PersistableSqlValue (q -> a) (a -> q)

-- | Run 'PersistableSqlValue' proof object. Convert from SQL type 'q' into Haskell type 'a'.
toValue   :: PersistableSqlValue q a -> q -> a
toValue   (PersistableSqlValue f _) = f

-- | Run 'PersistableSqlValue' proof object. Convert from Haskell type 'a' into SQL type 'q'.
fromValue :: PersistableSqlValue q a -> a -> q
fromValue (PersistableSqlValue _ g) = g

-- | Axiom of 'PersistableSqlValue' for SQL type 'q' and Haskell type 'a'.
persistableSqlValue :: PersistableSqlType q -> (q -> a) -> (a -> q) -> PersistableSqlValue q a
persistableSqlValue =  const PersistableSqlValue


-- | Proof object to specify width of Haskell type 'a'
--   when converting to SQL type list.
newtype PersistableRecordWidth a =
  PersistableRecordWidth { runPersistableRecordWidth :: Int }

-- | Unsafely generate 'PersistableRecordWidth' proof object from specified width of Haskell type 'a'.
persistableRecordWidth :: Int -> PersistableRecordWidth a
persistableRecordWidth =  PersistableRecordWidth

-- | Unsafely generate 'PersistableRecordWidth' proof object for Haskell type 'a' which is single field type.
valueWidth :: PersistableRecordWidth a
valueWidth =  persistableRecordWidth 1

-- | Derivation rule of 'PersistableRecordWidth' for tuple (,) type.
(<&>) :: PersistableRecordWidth a -> PersistableRecordWidth b -> PersistableRecordWidth (a, b)
a <&> b = PersistableRecordWidth $ runPersistableRecordWidth a + runPersistableRecordWidth b

-- | Derivation rule of 'PersistableRecordWidth' from from Haskell type 'a' into for Haskell type 'Maybe' 'a'.
maybeWidth :: PersistableRecordWidth a -> PersistableRecordWidth (Maybe a)
maybeWidth =  PersistableRecordWidth . runPersistableRecordWidth

-- | Axiom of 'PersistableRecordWidth' for Haskell unit () type.
voidWidth :: PersistableRecordWidth ()
voidWidth =  persistableRecordWidth 0


-- | Proof object which can bidirectionally convert bewteen Haskell type 'a' and list of SQL type ['q'].
data PersistableRecord q a =
  PersistableRecord (PersistableRecordWidth a) ([q] -> a) (a -> [q])

-- | Derivation rule of 'PersistableRecordWidth' for Haskell type 'a'.
--   'PersistableRecord' 'q' 'a' has width of Haskell type 'a'.
widthOfRecord :: PersistableRecord q a -> PersistableRecordWidth a
widthOfRecord (PersistableRecord w _ _) = w

-- | Run 'PersistableRecord' 'q' 'a' proof object. Convert from list of SQL type ['q'] into Haskell type 'a'.
toRecord :: PersistableRecord q a -> [q] -> a
toRecord      (PersistableRecord _ f _) = f

-- | Run 'PersistableRecord' 'q' 'a' proof object. Convert from Haskell type 'a' into list of SQL type ['q'].
fromRecord :: PersistableRecord q a -> a -> [q]
fromRecord    (PersistableRecord _ _ g) = g

-- | Get direct width value from 'PersistableRecord' 'q' 'a' proof.
width :: PersistableRecord q a -> Int
width =  runPersistableRecordWidth . widthOfRecord

-- | Run 'PersistableRecord' proof object.
--   Convert from list of SQL type ['q'] into Haskell type 'a' and rest list of SQL type ['q']
takeRecord :: PersistableRecord q a -> [q] -> (a, [q])
takeRecord rec vals = (toRecord rec va, vr) where
  (va, vr) = splitAt (width rec) vals

-- | Axiom of 'PersistableRecord' for SQL type 'q' and Haksell type 'a'.
persistableRecord :: PersistableRecordWidth a -> ([q] -> a) -> (a -> [q]) -> PersistableRecord q a
persistableRecord =  PersistableRecord

-- | Derivation rule of 'PersistableRecord' when Haskell type 'a' is single field type.
persistableFromValue :: PersistableRecordWidth a -> PersistableSqlValue q a -> PersistableRecord q a
persistableFromValue pw pv =
  persistableRecord pw (toValue pv . head) ((:[]) . fromValue pv)

-- | Axiom of 'PersistableRecord' for Haskell unit () type.
persistableVoid :: PersistableRecord q ()
persistableVoid =  persistableRecord voidWidth (const ()) (const [])


-- | Interface of inference rule for 'PersistableSqlType' proof object
class Eq q => PersistableType q where
  persistableType :: PersistableSqlType q

-- | Inferred Null value of SQL type.
sqlNullValue :: PersistableType q => q
sqlNullValue =  runPersistableNullValue persistableType


-- | Interface of inference rule for 'PersistableRecordWidth' proof object
class PersistableWidth a where
  persistableWidth :: PersistableRecordWidth a

-- | Inference rule of 'PersistableRecordWidth' proof object for tuple ('a', 'b') type.
instance (PersistableWidth a, PersistableWidth b) => PersistableWidth (a, b) where
  persistableWidth = persistableWidth <&> persistableWidth

-- | Inference rule of 'PersistableRecordWidth' proof object for 'Maybe' type.
instance PersistableWidth a => PersistableWidth (Maybe a) where
  persistableWidth = maybeWidth persistableWidth

-- | Inference rule of 'PersistableRecordWidth' for Haskell unit () type. Derive from axiom.
instance PersistableWidth () where
  persistableWidth = voidWidth


-- | Interface of inference rule for 'PersistableSqlValue' proof object
class PersistableType q => PersistableValue q a where
  persistableValue :: PersistableSqlValue q a

-- | Run inferred 'PersistableSqlValue' proof object. Convert from SQL type 'q' into Haskell type 'a'.
fromSql :: PersistableValue q a => q -> a
fromSql =  toValue persistableValue

-- | Run inferred 'PersistableSqlValue' proof object. Convert from Haskell type 'a' into SQL type 'q'.
toSql :: PersistableValue q a => a -> q
toSql =  fromValue persistableValue

-- | Inferred 'PersistableRecord' when Haskell type 'a' is single field type.
derivedPersistableValueRecord :: (PersistableWidth a, PersistableValue q a) => PersistableRecord q a
derivedPersistableValueRecord =  persistableFromValue persistableWidth persistableValue


-- | Interface of inference rule for 'PersistableRecord' proof object
class PersistableWidth a => Persistable q a where
  persistable :: PersistableRecord q a

-- | Axiom of 'PersistableRecord' for Haskell unit () type.
instance Persistable q () where
  persistable = persistableVoid

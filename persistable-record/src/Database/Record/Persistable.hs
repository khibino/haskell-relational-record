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
-- between Haskell type and list of SQL type.
module Database.Record.Persistable (
  -- * Specify SQL type
  PersistableSqlType, runPersistableNullValue, unsafePersistableSqlTypeFromNull,

  -- * Specify record width
  PersistableRecordWidth, runPersistableRecordWidth,
  unsafePersistableRecordWidth, unsafeValueWidth, (<&>), maybeWidth,

  -- * Bidirectional conversion between single column type and SQL type
  PersistableSqlValue, persistableSqlValue,
  toValue, fromValue,

  -- * Inference rules for proof objects

  PersistableType(..), sqlNullValue,
  PersistableValue (..), fromSql, toSql,
  PersistableWidth (..), derivedWidth
  ) where


-- | Proof object to specify type 'q' is SQL type
newtype PersistableSqlType q = PersistableSqlType q

-- | Null value of SQL type 'q'.
runPersistableNullValue :: PersistableSqlType q -> q
runPersistableNullValue (PersistableSqlType q) = q

-- | Unsafely generate 'PersistableSqlType' proof object from specified SQL null value which type is 'q'.
unsafePersistableSqlTypeFromNull :: q                    -- ^ SQL null value of SQL type 'q'
                                 -> PersistableSqlType q -- ^ Result proof object
unsafePersistableSqlTypeFromNull =  PersistableSqlType


-- | Proof object to specify value type 'a' is convertible with SQL type 'q'
data PersistableSqlValue q a = PersistableSqlValue (q -> a) (a -> q)

-- | Run 'PersistableSqlValue' proof object. Convert from SQL type 'q' into Haskell type 'a'.
toValue :: PersistableSqlValue q a -- ^ Proof object which has capability to convert
        -> q                       -- ^ SQL type
        -> a                       -- ^ Haskell type
toValue   (PersistableSqlValue f _) = f

-- | Run 'PersistableSqlValue' proof object. Convert from Haskell type 'a' into SQL type 'q'.
fromValue :: PersistableSqlValue q a -- ^ Proof object which has capability to convert
          -> a                       -- ^ Haskell type
          -> q                       -- ^ SQL type
fromValue (PersistableSqlValue _ g) = g

-- | Axiom of 'PersistableSqlValue' for SQL type 'q' and Haskell type 'a'.
persistableSqlValue :: PersistableSqlType q -> (q -> a) -> (a -> q) -> PersistableSqlValue q a
persistableSqlValue =  const PersistableSqlValue


-- | Proof object to specify width of Haskell type 'a'
--   when converting to SQL type list.
newtype PersistableRecordWidth a =
  PersistableRecordWidth Int

-- | Get width 'Int' value of record type 'a'.
runPersistableRecordWidth :: PersistableRecordWidth a -> Int
runPersistableRecordWidth (PersistableRecordWidth w) = w

-- | Unsafely generate 'PersistableRecordWidth' proof object from specified width of Haskell type 'a'.
unsafePersistableRecordWidth :: Int                      -- ^ Specify width of Haskell type 'a'
                             -> PersistableRecordWidth a -- ^ Result proof object
unsafePersistableRecordWidth =  PersistableRecordWidth

-- | Unsafely generate 'PersistableRecordWidth' proof object for Haskell type 'a' which is single column type.
unsafeValueWidth :: PersistableRecordWidth a
unsafeValueWidth =  unsafePersistableRecordWidth 1

-- | Derivation rule of 'PersistableRecordWidth' for tuple (,) type.
(<&>) :: PersistableRecordWidth a -> PersistableRecordWidth b -> PersistableRecordWidth (a, b)
a <&> b = PersistableRecordWidth $ runPersistableRecordWidth a + runPersistableRecordWidth b

-- | Derivation rule of 'PersistableRecordWidth' from from Haskell type 'a' into for Haskell type 'Maybe' 'a'.
maybeWidth :: PersistableRecordWidth a -> PersistableRecordWidth (Maybe a)
maybeWidth =  PersistableRecordWidth . runPersistableRecordWidth

-- | Axiom of 'PersistableRecordWidth' for Haskell unit () type.
voidWidth :: PersistableRecordWidth ()
voidWidth =  unsafePersistableRecordWidth 0


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

-- | Pass type parameter and inferred width value.
derivedWidth :: PersistableWidth a => (PersistableRecordWidth a, Int)
derivedWidth =  (pw, runPersistableRecordWidth pw) where
  pw = persistableWidth


-- | Interface of inference rule for 'PersistableSqlValue' proof object
class PersistableType q => PersistableValue q a where
  -- | Infer 'PersistableSqlValue' proof object.
  persistableValue :: PersistableSqlValue q a

-- | Run inferred 'PersistableSqlValue' proof object. Convert from SQL type 'q' into Haskell type 'a'.
fromSql :: PersistableValue q a => q -> a
fromSql =  toValue persistableValue

-- | Run inferred 'PersistableSqlValue' proof object. Convert from Haskell type 'a' into SQL type 'q'.
toSql :: PersistableValue q a => a -> q
toSql =  fromValue persistableValue

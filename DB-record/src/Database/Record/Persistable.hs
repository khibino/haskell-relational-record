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

  -- * Bidirectional conversion between record type and list of SQL type
  PersistableRecord, persistableRecord,
  toRecord, fromRecord, width, takeRecord,

  -- * Inference rules for proof objects
  persistableFromValue,

  PersistableType(..), sqlNullValue,
  PersistableValue (..), fromSql, toSql,
  derivedPersistableValueRecord,
  PersistableWidth (..), derivedWidth,
  Persistable (..)
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


-- | Proof object which can bidirectionally convert bewteen Haskell type 'a' and list of SQL type ['q'].
data PersistableRecord q a =
  PersistableRecord (PersistableRecordWidth a) ([q] -> a) (a -> [q])

-- | Derivation rule of 'PersistableRecordWidth' for Haskell type 'a'.
--   'PersistableRecord' 'q' 'a' has width of Haskell type 'a'.
widthOfRecord :: PersistableRecord q a    -- ^ Proof object which has capability to convert
              -> PersistableRecordWidth a -- ^ Result proof object
widthOfRecord (PersistableRecord w _ _) = w

-- | Run 'PersistableRecord' 'q' 'a' proof object. Convert from list of SQL type ['q'] into Haskell type 'a'.
toRecord :: PersistableRecord q a -- ^ Proof object which has capability to convert
         -> [q]                   -- ^ list of SQL type
         -> a                     -- ^ Haskell type
toRecord      (PersistableRecord _ f _) = f

-- | Run 'PersistableRecord' 'q' 'a' proof object. Convert from Haskell type 'a' into list of SQL type ['q'].
fromRecord :: PersistableRecord q a -- ^ Proof object which has capability to convert
           -> a                     -- ^ Haskell type
           -> [q]                   -- ^ list of SQL type
fromRecord    (PersistableRecord _ _ g) = g

-- | Get direct width value from 'PersistableRecord' 'q' 'a' proof object.
width :: PersistableRecord q a -> Int
width =  runPersistableRecordWidth . widthOfRecord

-- | Run 'PersistableRecord' proof object.
--   Convert from list of SQL type ['q'] into Haskell type 'a' and rest list of SQL type ['q']
takeRecord :: PersistableRecord q a -> [q] -> (a, [q])
takeRecord rec vals = (toRecord rec va, vr) where
  (va, vr) = splitAt (width rec) vals

-- | Axiom of 'PersistableRecord' for SQL type 'q' and Haksell type 'a'.
persistableRecord :: PersistableRecordWidth a -- ^ Proof object which specify width of Haskell type 'a'
                  -> ([q] -> a)               -- ^ Convert function body from SQL
                  -> (a -> [q])               -- ^ Convert function body into SQL
                  -> PersistableRecord q a    -- ^ Result proof object
persistableRecord =  PersistableRecord

-- | Derivation rule of 'PersistableRecord' when Haskell type 'a' is single column type.
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

-- | Inferred 'PersistableRecord' when Haskell type 'a' is single column type.
derivedPersistableValueRecord :: (PersistableWidth a, PersistableValue q a) => PersistableRecord q a
derivedPersistableValueRecord =  persistableFromValue persistableWidth persistableValue


-- | Interface of inference rule for 'PersistableRecord' proof object
class PersistableWidth a => Persistable q a where
  -- | Infer 'PersistableRecord' proof object.
  persistable :: PersistableRecord q a

-- | Axiom of 'PersistableRecord' for Haskell unit () type.
instance Persistable q () where
  persistable = persistableVoid

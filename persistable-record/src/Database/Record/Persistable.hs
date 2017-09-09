{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Database.Record.Persistable
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines proposition interfaces
-- for database value type and record type width.
module Database.Record.Persistable (
  -- * Specify database value type
  PersistableSqlType, runPersistableNullValue, unsafePersistableSqlTypeFromNull,

  -- * Specify record width
  PersistableRecordWidth, runPersistableRecordWidth,
  unsafePersistableRecordWidth, unsafeValueWidth, (<&>), maybeWidth,

  -- * Implicit derivation rules, database value type and record type width
  PersistableType(..), sqlNullValue,
  PersistableWidth (..), derivedWidth,

  -- * low-level interfaces
  GFieldWidthList,
  ProductConst, getProductConst,
  genericFieldOffsets,
  ) where

import GHC.Generics (Generic, Rep, U1 (..), K1 (..), M1 (..), (:*:)(..), to)
import Control.Applicative ((<$>), pure, Const (..))
import Data.Monoid (Monoid, Sum (..))
import Data.Array (Array, listArray, bounds, (!))
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Functor.ProductIsomorphic
  (ProductIsoFunctor, (|$|), ProductIsoApplicative, pureP, (|*|), )


-- | Proposition to specify type 'q' is database value type, contains null value
newtype PersistableSqlType q = PersistableSqlType q

-- | Null value of database value type 'q'.
runPersistableNullValue :: PersistableSqlType q -> q
runPersistableNullValue (PersistableSqlType q) = q

-- | Unsafely specify 'PersistableSqlType' axiom from specified database null value which type is 'q'.
unsafePersistableSqlTypeFromNull :: q                    -- ^ null value of database value type 'q'
                                 -> PersistableSqlType q -- ^ Result proof object
unsafePersistableSqlTypeFromNull =  PersistableSqlType


-- | Restricted in product isomorphism record type b
newtype ProductConst a b =
  ProductConst { unPC :: Const a b }
  deriving (ProductIsoFunctor, ProductIsoApplicative)

-- | extract constant value of 'ProductConst'.
getProductConst :: ProductConst a b -> a
getProductConst = getConst . unPC
{-# INLINE getProductConst #-}

-- | Proposition to specify width of Haskell type 'a'.
--   The width is length of database value list which is converted from Haskell type 'a'.
type PersistableRecordWidth a = ProductConst (Sum Int) a

-- unsafely map PersistableRecordWidth
pmap' :: Monoid e => (a -> b) -> ProductConst e a -> ProductConst e b
f `pmap'` prw = ProductConst $ f <$> unPC prw


-- | Get width 'Int' value of record type 'a'.
runPersistableRecordWidth :: PersistableRecordWidth a -> Int
runPersistableRecordWidth = getSum . getConst . unPC
{-# INLINE runPersistableRecordWidth #-}

instance Show a => Show (ProductConst a b) where
  show = ("PC " ++) . show . getConst . unPC

-- | Unsafely specify 'PersistableRecordWidth' axiom from specified width of Haskell type 'a'.
unsafePersistableRecordWidth :: Int                      -- ^ Specify width of Haskell type 'a'
                             -> PersistableRecordWidth a -- ^ Result proof object
unsafePersistableRecordWidth = ProductConst . Const . Sum
{-# INLINE unsafePersistableRecordWidth #-}

-- | Unsafely specify 'PersistableRecordWidth' axiom for Haskell type 'a' which is single column type.
unsafeValueWidth :: PersistableRecordWidth a
unsafeValueWidth =  unsafePersistableRecordWidth 1
{-# INLINE unsafeValueWidth #-}

-- | Derivation rule of 'PersistableRecordWidth' for tuple (,) type.
(<&>) :: PersistableRecordWidth a -> PersistableRecordWidth b -> PersistableRecordWidth (a, b)
a <&> b = (,) |$| a |*| b

-- | Derivation rule of 'PersistableRecordWidth' from from Haskell type 'a' into for Haskell type 'Maybe' 'a'.
maybeWidth :: PersistableRecordWidth a -> PersistableRecordWidth (Maybe a)
maybeWidth = pmap' Just


-- | Interface of derivation rule for 'PersistableSqlType'.
class Eq q => PersistableType q where
  persistableType :: PersistableSqlType q

-- | Implicitly derived null value of database value type.
sqlNullValue :: PersistableType q => q
sqlNullValue =  runPersistableNullValue persistableType


{- |
'PersistableWidth' 'a' is implicit rule to derive 'PersistableRecordWidth' 'a' width proposition for type 'a'.

Generic programming (<https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#generic-programming>)
with default signature is available for 'PersistableWidth' class,
so you can make instance like below:

@
  \{\-\# LANGUAGE DeriveGeneric \#\-\}
  import GHC.Generics (Generic)
  --
  data Foo = Foo { ... } deriving Generic
  instance PersistableWidth Foo
@

-}
class PersistableWidth a where
  persistableWidth :: PersistableRecordWidth a

  default persistableWidth :: (Generic a, GFieldWidthList (Rep a)) => PersistableRecordWidth a
  persistableWidth = pmapConst (Sum . lastA) genericFieldOffsets
    where
      lastA a = a ! (snd $ bounds a)


pmapConst :: (a -> b) -> ProductConst a c -> ProductConst b c
pmapConst f = ProductConst . Const . f . getConst . unPC

-- | Generic width value list of record fields.
class GFieldWidthList f where
  gFieldWidthList :: ProductConst (DList Int) (f a)

instance GFieldWidthList U1 where
  gFieldWidthList = pureP U1

instance (GFieldWidthList a, GFieldWidthList b) => GFieldWidthList (a :*: b) where
  gFieldWidthList = (:*:) |$| gFieldWidthList |*| gFieldWidthList

instance GFieldWidthList a => GFieldWidthList (M1 i c a) where
  gFieldWidthList = M1 |$| gFieldWidthList

instance PersistableWidth a => GFieldWidthList (K1 i a) where
  gFieldWidthList = K1 |$| pmapConst (pure . getSum) persistableWidth

offsets :: [Int] -> Array Int Int
offsets ws = listArray (0, length ws) $ scanl (+) 0 ws

-- | Generic offset array of record fields.
genericFieldOffsets :: (Generic a, GFieldWidthList (Rep a)) => ProductConst (Array Int Int) a
genericFieldOffsets = pmapConst (offsets . DList.toList) $ to `pmap'` gFieldWidthList


-- | Inference rule of 'PersistableRecordWidth' proof object for 'Maybe' type.
instance PersistableWidth a => PersistableWidth (Maybe a) where
  persistableWidth = maybeWidth persistableWidth

-- | Inference rule of 'PersistableRecordWidth' for Haskell unit () type. Derive from axiom.
instance PersistableWidth ()  -- default generic instance

-- | Pass type parameter and inferred width value.
derivedWidth :: PersistableWidth a => (PersistableRecordWidth a, Int)
derivedWidth =  (pw, runPersistableRecordWidth pw) where
  pw = persistableWidth

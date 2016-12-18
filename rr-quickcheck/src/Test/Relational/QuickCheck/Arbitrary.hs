{-# LANGUAGE FlexibleInstances #-}

module Test.Relational.QuickCheck.Arbitrary (
  Selector (..), Ranged (..),
  ) where

import Test.QuickCheck (Arbitrary (..), Gen, elements, listOf)
import Control.Applicative ((<$>), (<*>))
import Data.Int (Int64)
import Database.Relational.Query
import Database.Relational.Query.Pi.Unsafe (unsafeExpandIndexes)

import Test.Relational.QuickCheck.Model


data Selector r =
  Selector
  { int  :: r -> Int64
  , sql  :: Pi r Int64
  }

instance Show (Selector r) where
  show s = unwords ["Selector", show . unsafeExpandIndexes $ sql s]

genSelector :: [(r -> Int64, Pi r Int64)] -> Gen (Selector r)
genSelector = elements . map (uncurry Selector)

instance Arbitrary (Selector A) where
  arbitrary = genSelector [ (a0, a0'), (a1, a1'), (a2, a2') ]

instance Arbitrary (Selector B) where
  arbitrary = genSelector [ (b0, b0'), (b1, b1'), (b2, b2') ]

newtype Ranged a = Ranged { runRanged :: [a] }
  deriving (Eq, Show)

range10 :: (Num a, Enum a) => Gen a
range10 = elements [0 .. 10]

instance Arbitrary (Ranged A) where
  arbitrary =
    (Ranged <$>) . listOf $
    A
    <$> range10
    <*> range10
    <*> range10

instance Arbitrary (Ranged B) where
  arbitrary =
    (Ranged <$>) . listOf $
    B
    <$> range10
    <*> range10
    <*> range10

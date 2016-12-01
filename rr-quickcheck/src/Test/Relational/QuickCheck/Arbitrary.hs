{-# LANGUAGE FlexibleInstances #-}

module Test.Relational.QuickCheck.Arbitrary (
  Selector (..),
  ) where

import Test.QuickCheck (Arbitrary (..), elements)
import Data.Int (Int64)
import Database.Relational.Query

import Test.Relational.QuickCheck.Model


data Selector r =
  Selector
  { pure :: r -> Int64
  , sql  :: Pi r Int64
  }

instance Arbitrary (Selector A) where
  arbitrary = elements $
              map (uncurry Selector)
              [ (a0, a0'), (a1, a1'), (a2, a2') ]

instance Arbitrary (Selector B) where
  arbitrary = elements $
              map (uncurry Selector)
              [ (b0, b0'), (b1, b1'), (b2, b2') ]

{-# OPTIONS -fno-warn-orphans #-}

module MonoidLaw (tests) where

import Language.SQL.Keyword (Keyword, DString)

import Data.Monoid (Monoid, mempty, (<>))
import Data.String (fromString)
import Distribution.TestSuite.Compat (TestList, testList, prop)
import Test.QuickCheck (Arbitrary (..))


leftId :: (Eq a, Monoid a) => a -> Bool
leftId a = mempty <> a == a

rightId :: (Eq a, Monoid a) => a -> Bool
rightId a = a <> mempty == a

assoc :: (Eq a, Monoid a) => a -> a -> a -> Bool
assoc a b c = (a <> b) <> c == a <> (b <> c)

dsLeftId :: DString -> Bool
dsLeftId =  leftId

dsRightId :: DString -> Bool
dsRightId =  rightId

dsAssoc :: DString -> DString -> DString -> Bool
dsAssoc =  assoc

instance Arbitrary DString where
  arbitrary = fmap read arbitrary

kwLeftId :: Keyword -> Bool
kwLeftId =  leftId

kwRightId :: Keyword -> Bool
kwRightId =  rightId

kwAssoc :: Keyword -> Keyword -> Keyword -> Bool
kwAssoc =  assoc

instance Arbitrary Keyword where
  arbitrary = fmap fromString arbitrary

tests :: TestList
tests =  testList [ prop "DString left Id"  dsLeftId
                  , prop "DString right Id" dsRightId
                  , prop "DString associativity" dsAssoc
                  , prop "Keyword left Id"  kwLeftId
                  , prop "Keyword right Id" kwRightId
                  , prop "Keyword associativity" kwAssoc
                  ]

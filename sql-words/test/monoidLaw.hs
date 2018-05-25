{-# OPTIONS -fno-warn-orphans #-}

import Language.SQL.Keyword
  (Keyword (Sequence), DString, (<++>))

import Data.Monoid (Monoid, mempty, (<>))
import Data.String (fromString)
import Test.QuickCheck (Arbitrary (..), Testable)
import Test.QuickCheck.Simple (Test, qcTest, defaultMain)


prop :: Testable prop => String -> prop -> Test
prop = qcTest

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

concatCommutative :: DString -> DString -> Bool
concatCommutative x y =
  Sequence x <++> Sequence y
  ==
  Sequence (x <> y)

tests :: [Test]
tests =  [ prop "DString left Id"  dsLeftId
         , prop "DString right Id" dsRightId
         , prop "DString associativity" dsAssoc
         , prop "Keyword left Id"  kwLeftId
         , prop "Keyword right Id" kwRightId
         , prop "Keyword associativity" kwAssoc
         , prop "concat commutative" concatCommutative
         ]

main :: IO ()
main = defaultMain tests

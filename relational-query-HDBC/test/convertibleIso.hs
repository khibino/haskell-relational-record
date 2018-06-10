{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Int (Int8, Int16)
import Data.Convertible (Convertible, safeConvert, ConvertResult)
import Database.HDBC (SqlValue (SqlInteger))

import Test.QuickCheck (Arbitrary (..), resize)
import Test.QuickCheck.Simple (qcTest, defaultMain)

import Database.HDBC.Record.Persistable ()



prop_toFromQvInt8 :: Int8 -> Bool
prop_toFromQvInt8 i8 = Right i8 == (safeConvert =<< sv)
  where
    sv :: ConvertResult SqlValue
    sv = safeConvert i8

prop_toFromQvInt16 :: Int16 -> Bool
prop_toFromQvInt16 i16 = Right i16 == (safeConvert =<< sv)
  where
    sv :: ConvertResult SqlValue
    sv = safeConvert i16


newtype IntegerR i =
  IntegerR Integer
  deriving (Eq, Show)

instance Arbitrary (IntegerR Int8) where
  arbitrary = IntegerR <$> resize 1000 arbitrary

instance Arbitrary (IntegerR Int16) where
  arbitrary = IntegerR <$> resize 100000 arbitrary


prop_fromToQvBounded :: (Integral a, Convertible SqlValue a, Convertible a SqlValue)
                     => a -> a -> IntegerR a -> Bool
prop_fromToQvBounded mn' mx' (IntegerR i)
  | i < mn || mx < i  =  True
  | otherwise         =  Right sv == (safeConvert =<< ix)
  where
    sv = SqlInteger i
    ix = safeConvert sv `asTypeOf` Right mn'
    mn = fromIntegral mn'
    mx = fromIntegral mx'

prop_fromToQvInt8 :: IntegerR Int8 -> Bool
prop_fromToQvInt8 = prop_fromToQvBounded minBound maxBound

prop_fromToQvInt16 :: IntegerR Int16 -> Bool
prop_fromToQvInt16 = prop_fromToQvBounded minBound maxBound

main :: IO ()
main =
  defaultMain
  [ qcTest "int8  - to SqlV from SqlV" prop_toFromQvInt8
  , qcTest "int16 - to SqlV from SqlV" prop_toFromQvInt16
  , qcTest "int8  - from SqlV to SqlV" prop_fromToQvInt8
  , qcTest "int16 - from SqlV to SqlV" prop_fromToQvInt16
  ]

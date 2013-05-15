{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS -fno-warn-orphans #-}

-- |
-- Module      : Database.HDBC.SqlValueExtra
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.HDBC.SqlValueExtra () where

import Data.Convertible (Convertible(safeConvert), ConvertResult)
import Data.Int (Int16, Int32)
import Database.HDBC (SqlValue)

safeConvertFromIntegral32 :: Integral a => a -> ConvertResult SqlValue
safeConvertFromIntegral32 i =
  safeConvert (fromIntegral i :: Int32)

safeConvertToIntegral32 :: Integral a => SqlValue -> ConvertResult a
safeConvertToIntegral32 v =
  fmap fromIntegral (safeConvert v :: ConvertResult Int32)

instance Convertible Int16 SqlValue where
  safeConvert = safeConvertFromIntegral32

instance Convertible SqlValue Int16 where
  safeConvert = safeConvertToIntegral32

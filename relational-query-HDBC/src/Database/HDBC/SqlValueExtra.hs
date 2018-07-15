{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -fno-warn-orphans #-}

-- |
-- Module      : Database.HDBC.SqlValueExtra
-- Copyright   : 2013-2018 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.HDBC.SqlValueExtra () where

import Data.Convertible (Convertible(safeConvert), ConvertResult)
import Data.Int (Int8, Int16, Int32)
import Data.Word (Word8, Word16)
import Database.HDBC (SqlValue)

-- Convert from narrower width than Int32
safeConvertFromIntegral32 :: Integral a => a -> ConvertResult SqlValue
safeConvertFromIntegral32 i =
  safeConvert (fromIntegral i :: Int32)

safeConvertToIntegral32 :: Convertible Int32 a => SqlValue -> ConvertResult a
safeConvertToIntegral32 v =
  safeConvert =<< (safeConvert v :: ConvertResult Int32)

instance Convertible Int8 SqlValue where
  safeConvert = safeConvertFromIntegral32

instance Convertible SqlValue Int8 where
  safeConvert = safeConvertToIntegral32

instance Convertible Int16 SqlValue where
  safeConvert = safeConvertFromIntegral32

instance Convertible SqlValue Int16 where
  safeConvert = safeConvertToIntegral32

instance Convertible Word8 SqlValue where
  safeConvert = safeConvertFromIntegral32

instance Convertible SqlValue Word8 where
  safeConvert = safeConvertToIntegral32

instance Convertible Word16 SqlValue where
  safeConvert = safeConvertFromIntegral32

instance Convertible SqlValue Word16 where
  safeConvert = safeConvertToIntegral32

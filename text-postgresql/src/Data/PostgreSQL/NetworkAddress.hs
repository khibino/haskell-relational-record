-- |
-- Module      : Data.PostgreSQL.NetworkAddress
-- Copyright   : 2015-2018 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines network-address types of PostgreSQL.
-- http://www.postgresql.org/docs/current/static/datatype-net-types.html
module Data.PostgreSQL.NetworkAddress
       ( NetAddress (..), netAddress4, netAddress6
       , V4HostAddress (..), v4HostAddressOctets
       , V6HostAddress (..), v6HostAddressLong, v6HostAddressWords
       , v6HostAddress, v6HostAddressL, v6HostAddressR

       , Inet (..), Cidr (..)
       ) where

import Control.Applicative (pure)
import Control.Monad (guard)
import Data.Word (Word8, Word16)


-- | Host address type along with IPv4 address string.
data V4HostAddress =
  V4HostAddress !Word8 !Word8 !Word8 !Word8
  deriving (Eq, Ord, Show, Read)

v4HostAddressOctets :: V4HostAddress -> (Word8, Word8, Word8, Word8)
v4HostAddressOctets (V4HostAddress a b c d) = (a, b, c, d)


-- | Host address type along with IPv6 address string.
--   Each 'Word16' value is host byte order.
--   Host byte order is portable in programs on its own host.
--   Network byte order is only needed, when communicating other hosts.
data V6HostAddress =
  V6HostAddress !Word16 !Word16 !Word16 !Word16 !Word16 !Word16 !Word16 !Word16
  deriving (Eq, Ord, Show, Read)

v6HostAddressLong :: Word16 -> Word16 -> Word16 -> Word16
                  -> Word16 -> Word16 -> Word16 -> Word16
                  -> V6HostAddress
v6HostAddressLong = V6HostAddress

v6HostAddress :: [Word16] -> [Word16] -> Maybe V6HostAddress
v6HostAddress ls rs  = do
  let v6length = 8
  guard . null . drop v6length $ ls ++ rs
  [a, b, c, d, e, f, g, h]  <-  pure $ ls ++ replicate (v6length - length ls - length rs) 0 ++ rs
  pure $ v6HostAddressLong a b c d e f g h

v6HostAddressR :: [Word16] -> Maybe V6HostAddress
v6HostAddressR = v6HostAddress []

v6HostAddressL :: [Word16] -> Maybe V6HostAddress
v6HostAddressL ls = v6HostAddress ls []

v6HostAddressWords :: V6HostAddress -> (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16)
v6HostAddressWords (V6HostAddress a b c d e f g h) =
  (a, b, c, d, e, f, g, h)


data NetAddress
  = NetAddress4 !V4HostAddress !Word8
  | NetAddress6 !V6HostAddress !Word8
  deriving (Eq, Ord, Show, Read)

netAddress4 :: V4HostAddress -> Word8 -> Maybe NetAddress
netAddress4 a4 m
  | 0 <= m && m <= 32  =  Just $ NetAddress4 a4 m
  | otherwise          =  Nothing

netAddress6 :: V6HostAddress -> Word8 -> Maybe NetAddress
netAddress6 a6 m
  | 0 <= m && m <= 128 =  Just $ NetAddress6 a6 m
  | otherwise          =  Nothing

newtype Inet = Inet NetAddress  deriving (Eq, Ord, Show, Read)

newtype Cidr = Cidr NetAddress  deriving (Eq, Ord, Show, Read)

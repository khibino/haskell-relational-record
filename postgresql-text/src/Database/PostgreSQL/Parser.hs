
module Database.PostgreSQL.Parser
       ( Parser, evalParser
       , netAddress
       , hostAddress, decMask4
       , v6HostAddress, decMask6
       ) where

import Control.Applicative ((<$>), pure, (<*>), (<*), (*>), (<|>), many, some, optional)
import Control.Monad (guard, replicateM)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Char (isDigit, isHexDigit)
import Data.Word (Word8, Word16)
import Numeric (readDec, readHex)

import Text.Parser.List (evalParser, eof, noteP, satisfy', satisfy)
import qualified Text.Parser.List as P
import Data.PostgreSQL.NetworkAddress (NetAddress (..), HostAddress, V6HostAddress)
import qualified Data.PostgreSQL.NetworkAddress as D


type Parser = P.Parser Char

digit :: Parser Char
digit = satisfy' "digit" (const "must be digit.") isDigit

hexDigit :: Parser Char
hexDigit = satisfy' "hexDigit" (const "must be hex-digit.") isHexDigit

readNat :: String -> Maybe Integer
readNat s = listToMaybe [ i | (i, "") <- readDec s ] -- readDec accept only positive

readHexNat :: String -> Maybe Integer
readHexNat s = listToMaybe [ i | (i, "") <- readHex s ]

nat :: Parser Integer
nat = do
  xs <- some digit
  noteP "nat: invalid input" $ readNat xs

hexNat :: Parser Integer
hexNat = do
  xs <- some hexDigit
  noteP "hexNat: invalid input" $ readHexNat xs

rangedNat :: (Integral a, Show a) => a -> a -> Integer -> Parser a
rangedNat n x i = do
  noteP (concat ["rangedNat: out of range: ", show i, ": [", show n, ", ", show x, "]"])
    . guard $ (fromIntegral n <= i && i <= fromIntegral x)
  pure $ fromIntegral i

decW8 :: Parser Word8
decW8 = rangedNat minBound maxBound =<< nat

hexW16 :: Parser Word16
hexW16 = rangedNat minBound maxBound =<< hexNat

char :: Char -> Parser Char
char c = satisfy (== c)

dot :: Parser Char
dot = char '.'

colon :: Parser Char
colon = char ':'

slash :: Parser Char
slash = char '/'

hostAddress :: Parser HostAddress
hostAddress = D.hostAddress <$> decW8 <* dot <*> decW8 <* dot <*> decW8 <* dot <*> decW8

_exampleHostAddress :: [Either String HostAddress]
_exampleHostAddress =
  [ evalParser (hostAddress <* eof) s
  | s <- [ "0.0.0.0", "192.168.0.1" ]
  ]

mask4bits :: Word8
mask4bits = 32

decMask4 :: Parser Word8
decMask4 = rangedNat 0 mask4bits =<< nat

v6words :: Parser [Word16]
v6words =
  (:) <$> hexW16 <*> many (colon *> hexW16)   <|>
  pure []

doubleColon6 :: Parser V6HostAddress
doubleColon6 = do
  m6  <-  D.v6HostAddress <$> v6words <* replicateM 2 colon <*> v6words
  noteP "v6HostAddress: Too many numbers of 16-bit words." m6

v6HostAddress :: Parser V6HostAddress
v6HostAddress =
  doubleColon6                             <|>
  D.v6HostAddressLong
  <$> hexW16 <* colon <*> hexW16 <* colon
  <*> hexW16 <* colon <*> hexW16 <* colon
  <*> hexW16 <* colon <*> hexW16 <* colon
  <*> hexW16 <* colon <*> hexW16

_exampleHostAddress6 :: [Either String V6HostAddress]
_exampleHostAddress6 =
  [ evalParser (v6HostAddress <* eof) s
  | s <- [ "::", "0:0:0:0:0:0:0:0", "2001:1::1:a2", "1:1:1:1:1:1:1:a1" ]
  ]

mask6bits :: Word8
mask6bits = 128

decMask6 :: Parser Word8
decMask6 = rangedNat 0 mask6bits =<< nat

optional' :: a -> Parser a -> Parser a
optional' x p =
  fromMaybe x <$> optional p

netAddress :: Parser NetAddress
netAddress =
  NetAddress4 <$> hostAddress   <*> optional' mask4bits (slash *> decMask4) <|>
  NetAddress6 <$> v6HostAddress <*> optional' mask6bits (slash *> decMask6)

_exampleNetAddress :: [Either String NetAddress]
_exampleNetAddress =
  [ evalParser (netAddress <* eof) s
  | s <- [ "2001:1::a0:a2/64", "172.16.0.0" ]
  ]

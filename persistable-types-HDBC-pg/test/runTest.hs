{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Control.Applicative ((<$>), (*>))
import Control.Monad (unless)
import Database.Relational.HDBC.TH (makeRelationalRecord)
import Data.PostgreSQL.NetworkAddress
  (Inet (..), Cidr (..), NetAddress (..),
   V4HostAddress (..), V6HostAddress (..))
import Database.Relational (Relation, relation, value)

import Database.Relational.HDBC.PostgreSQL ()


data Foo =
  Foo
  { s0 :: String
  , i1 :: Maybe Inet
  , c2 :: Maybe Cidr
  } deriving Generic

-- compile time check
$(makeRelationalRecord ''Foo)

-- 192.168.0.1/24
inet4 :: Relation () Inet
inet4 =
  relation . return . value . Inet $
  NetAddress4 (V4HostAddress 192 168 0 1) 24

-- 224.0.0.0/4
cidr4 :: Relation () Cidr
cidr4 =
  relation . return . value . Cidr $
  NetAddress4 (V4HostAddress 224 0 0 0) 4

-- fd00::1/8
inet6 :: Relation () Inet
inet6 =
  relation . return . value . Inet $
  NetAddress6 (V6HostAddress 0xfd00 0 0 0 0 0 0 1) 8

-- ff00::/8
cidr6 :: Relation () Cidr
cidr6 =
  relation . return . value . Cidr $
  NetAddress6 (V6HostAddress 0xff00 0 0 0 0 0 0 0) 8

testSet :: [(String, String, [String])]
testSet =
  [("inet - v4", show inet4, ["SELECT","ALL","INET","'192.168.0.1/24'","AS","f0"]),
   ("cidr - v4", show cidr4, ["SELECT","ALL","CIDR","'224.0.0.0/4'","AS","f0"]),
   ("inet - v6", show inet6, ["SELECT","ALL","INET","'fd00:0:0:0:0:0:0:1/8'","AS","f0"]),
   ("cidr - v6", show cidr6, ["SELECT","ALL","CIDR","'ff00:0:0:0:0:0:0:0/8'","AS","f0"])]

doTest :: (String, String, [String]) -> IO Bool
doTest (tag, q, a)
  | words q == a  =  putStrLn ("success: " ++ tag) *> return True
  | otherwise     =  putStrLn ("failed: " ++ tag) *> return False

main :: IO ()
main = do
  ok <- and <$> mapM doTest testSet
  unless ok $ fail "some tests are failed!"

{-# OPTIONS -fno-warn-orphans #-}
{--# LANGUAGE FlexibleInstances #--}

import Test.QuickCheck
  (Gen, Arbitrary (..), choose, oneof)
import Test.QuickCheck.Simple (defaultMain, Test, qcTest)

import Control.Applicative ((<$>), (<*>))
import Data.Word (Word8)

import Data.PostgreSQL.NetworkAddress
import Database.PostgreSQL.Parser (Parser, evalParser)
import qualified Database.PostgreSQL.Parser as Parser
import Database.PostgreSQL.Printer (Printer, execPrinter)
import qualified Database.PostgreSQL.Printer as Printer


instance Arbitrary V4HostAddress where
  arbitrary =
    V4HostAddress
    <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary V6HostAddress where
  arbitrary =
    V6HostAddress
    <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

mask4 :: Gen Word8
mask4 = choose (0, 32)

mask6 :: Gen Word8
mask6 = choose (0, 128)

instance Arbitrary NetAddress where
  arbitrary =
    oneof
    [ NetAddress4 <$> arbitrary <*> mask4
    , NetAddress6 <$> arbitrary <*> mask6 ]

isoProp :: Eq a => Printer a -> Parser a -> a -> Bool
isoProp pr ps a =
  Right a == (evalParser ps $ execPrinter pr a)

prop_v4HostAddressIso :: V4HostAddress -> Bool
prop_v4HostAddressIso =
  isoProp Printer.v4HostAddress Parser.v4HostAddress

prop_v6HostAddressIso :: V6HostAddress -> Bool
prop_v6HostAddressIso =
  isoProp Printer.v6HostAddress Parser.v6HostAddress

prop_netAddressPpIso :: NetAddress -> Bool
prop_netAddressPpIso =
  isoProp Printer.netAddress Parser.netAddress

prop_netAddressDcIso :: NetAddress -> Bool
prop_netAddressDcIso na =
  Just na == (case na of
                NetAddress4 a4 m  ->  netAddress4 a4 m
                NetAddress6 a6 m  ->  netAddress6 a6 m)

tests :: [Test]
tests =
  [ qcTest "v4 address iso - print parse"      prop_v4HostAddressIso
  , qcTest "v6 address iso - print parse"      prop_v6HostAddressIso
  , qcTest "network address iso - print parse" prop_netAddressPpIso
  , qcTest "network address iso - destruct construct" prop_netAddressDcIso
  ]

main :: IO ()
main = defaultMain tests

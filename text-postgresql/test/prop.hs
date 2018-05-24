{-# OPTIONS -fno-warn-orphans #-}

import Test.QuickCheck
  (Gen, Arbitrary (..), choose, oneof)
import Test.QuickCheck.Simple (defaultMain, Test, qcTest)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Word (Word8, Word16)

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

newtype A6Input =
  A6Input [Word16]
  deriving (Eq, Show)

instance Arbitrary A6Input where
  arbitrary= A6Input <$> (choose (0, 8) >>= (`replicateM` arbitrary))

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

prop_v6HostAddressDcIsoL :: V6HostAddress -> Bool
prop_v6HostAddressDcIsoL a6 =
    v6HostAddress [w0, w1, w2, w3, w4, w5, w6, w7] [] == Just a6
  where
    (w0, w1, w2, w3, w4, w5, w6, w7) = v6HostAddressWords a6

prop_v6HostAddressDcIsoR :: V6HostAddress -> Bool
prop_v6HostAddressDcIsoR a6 =
    v6HostAddress [] [w0, w1, w2, w3, w4, w5, w6, w7] == Just a6
  where
    (w0, w1, w2, w3, w4, w5, w6, w7) = v6HostAddressWords a6

prop_v6HostAddressCons :: A6Input -> A6Input -> Bool
prop_v6HostAddressCons (A6Input il) (A6Input ir) = case v6HostAddress il ir of
  Nothing  ->  length (il ++ ir) > 8
  Just (V6HostAddress w0 w1 w2 w3 w4 w5 w6 w7)
    | let ws = [w0, w1, w2, w3, w4, w5, w6, w7] ->
      length (il ++ ir) <= 8 && il `isPrefixOf` ws && ir `isSuffixOf` ws

prop_netAddressPpIso :: NetAddress -> Bool
prop_netAddressPpIso =
  isoProp Printer.netAddress Parser.netAddress

prop_netAddressDcIso :: NetAddress -> Bool
prop_netAddressDcIso na = dc == Just na  where
 dc = case na of
        NetAddress4 a4 m  ->  netAddress4 a4 m
        NetAddress6 a6 m  ->  netAddress6 a6 m

prop_netAddress4Cons :: V4HostAddress -> Word8 -> Bool
prop_netAddress4Cons a4 m = case netAddress4 a4 m of
  Nothing                   ->  m > 32
  Just (NetAddress4 a4' m') ->  a4 == a4' && m == m'
  Just (NetAddress6 {})     ->  False

prop_netAddress6Cons :: V6HostAddress -> Word8 -> Bool
prop_netAddress6Cons a6 m = case netAddress6 a6 m of
  Nothing                   ->  m > 128
  Just (NetAddress4 {})     ->  False
  Just (NetAddress6 a6' m') ->  a6 == a6' && m == m'

prop_cidr4Cons :: V4HostAddress -> Word8 -> Bool
prop_cidr4Cons a4 m = case cidr4 a4 m of
  Nothing  ->  m > 32 ||
               case cidr4' a4 m of
                 Nothing  ->  False
                 Just (Cidr (NetAddress4 a4' m')) ->  m' == m && a4' /= a4
                 Just (Cidr (NetAddress6 {}))     ->  False
  Just (Cidr (NetAddress4 a4' m'))                ->  m' == m && a4' == a4
  Just (Cidr (NetAddress6 {}))                    ->  False

prop_cidr6Cons :: V6HostAddress -> Word8 -> Bool
prop_cidr6Cons a6 m = case cidr6 a6 m of
  Nothing  ->  m > 128 ||
               case cidr6' a6 m of
                 Nothing  ->  False
                 Just (Cidr (NetAddress4 {}))     ->  False
                 Just (Cidr (NetAddress6 a6' m')) ->  m' == m && a6' /= a6
  Just (Cidr (NetAddress4 {}))                    ->  False
  Just (Cidr (NetAddress6 a6' m'))                ->  m' == m && a6' == a6

tests :: [Test]
tests =
  [ qcTest "v4 address iso - print parse"      prop_v4HostAddressIso
  , qcTest "v6 address iso - print parse"      prop_v6HostAddressIso
  , qcTest "v6 address iso - destruct construct-left"  prop_v6HostAddressDcIsoL
  , qcTest "v6 address iso - destruct construct-right" prop_v6HostAddressDcIsoR
  , qcTest "v6 address construction"           prop_v6HostAddressCons
  , qcTest "network address iso - print parse" prop_netAddressPpIso
  , qcTest "network address iso - destruct construct" prop_netAddressDcIso
  , qcTest "network address 4 construction"    prop_netAddress4Cons
  , qcTest "network address 6 construction"    prop_netAddress6Cons
  , qcTest "cidr-4 construction"               prop_cidr4Cons
  , qcTest "cidr-6 construction"               prop_cidr6Cons
  ]

main :: IO ()
main = defaultMain tests

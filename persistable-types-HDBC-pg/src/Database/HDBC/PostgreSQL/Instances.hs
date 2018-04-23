{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.HDBC.PostgreSQL.Instances () where

import Control.Applicative ((<$>), pure, (<*))
import Data.String (IsString, fromString)
import Data.ByteString.Char8 (unpack)
import Data.Convertible (Convertible (..), ConvertResult, ConvertError (..))
import Data.PostgreSQL.NetworkAddress (NetAddress, Inet (..), Cidr (..))
import Database.HDBC (SqlValue (..))
import Database.HDBC.Record.Persistable ()
import Database.PostgreSQL.Parser (evalParser)
import qualified Database.PostgreSQL.Parser as Parser
import Database.PostgreSQL.Printer (execPrinter)
import qualified Database.PostgreSQL.Printer as Printer


note :: a -> Maybe b -> Either a b
note e = maybe (Left e) Right

mapConvert :: Show a => String -> String -> a -> Either String b -> ConvertResult b
mapConvert srcT destT sv = either (Left . mke) Right  where
  mke em =
    ConvertError
    { convSourceValue   =  show sv
    , convSourceType    =  srcT
    , convDestType      =  destT
    , convErrorMessage  =  em
    }

takeAddressString :: SqlValue -> Maybe String
takeAddressString = d  where
  d (SqlString s)      =  Just s
  d (SqlByteString s)  =  Just $ unpack s
  d  _                 =  Nothing

toNetAddress :: SqlValue -> ConvertResult NetAddress
toNetAddress qv = mapConvert "SqlValue" "NetAddress" qv $ do
  s  <-  note "Fail to take address string from the column value."
         $ takeAddressString qv
  evalParser (Parser.netAddress <* Parser.eof) s

instance Convertible SqlValue Inet where
  safeConvert = (Inet <$>) . toNetAddress

instance Convertible SqlValue Cidr where
  safeConvert = (Cidr <$>) . toNetAddress

fromNetAddress :: NetAddress -> ConvertResult SqlValue
fromNetAddress = pure . SqlString . execPrinter Printer.netAddress

instance Convertible Inet SqlValue where
  safeConvert (Inet n) = fromNetAddress n

instance Convertible Cidr SqlValue where
  safeConvert (Cidr n) = fromNetAddress n

showNetAddr :: IsString s => NetAddress -> s
showNetAddr = fromString . execPrinter Printer.netAddress

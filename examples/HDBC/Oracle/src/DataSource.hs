{-# OPTIONS_GHC -fno-warn-orphans #-}

module DataSource where

import Control.Applicative ((<$>))
import Database.HDBC.ODBC (Connection, connectODBC)

data Option = Option
    { dsn :: String
    , uid :: String
    , pwd :: String
    } deriving (Show, Read)

data Param = Param
     { option :: Option
     , owner  :: String
     } deriving (Show, Read)

dsString :: Option -> String
dsString (Option d u p) =
  concat
  [ "DSN=", d, ";"
  , "UID=", u, ";"
  , "PWD=", p
  ]

getParam :: IO Param
getParam = readIO =<< readFile "datasource.show"

connect :: IO Connection
connect = connectODBC . dsString . option =<< getParam

getOwner :: IO String
getOwner = owner <$> getParam

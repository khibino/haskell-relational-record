module DataSource where

import Control.Applicative ((<$>), (<*>))
import System.IO.Unsafe (unsafePerformIO)

import Database.HDBC.ODBC (Connection, connectODBC)

data Option = Option
    { dsn :: String
    , uid :: String
    , pwd :: String
    }

instance Show Option where
    show (Option d u p) = concat
        [ "DSN=", d, ";"
        , "UID=", u, ";"
        , "PWD=", p
        ]

getOption :: IO Option
getOption = Option <$> get "DSN: " <*> get "UID: " <*> get "PWD: "
  where
    get str = putStr str >> getLine

connect :: IO Connection
connect = do
    option <- getOption
    connectODBC $ show option

owner :: String
owner = unsafePerformIO $ do
    putStr "OWNER: "
    getLine

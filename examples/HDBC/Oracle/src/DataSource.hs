module DataSource where

import Control.Applicative ((<$>), (<*>))
import System.IO (stdin, stdout, hSetBuffering, hPutStr, hGetLine, BufferMode (NoBuffering))
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

get :: String -> IO String
get str = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hPutStr stdout str
    hGetLine stdin

getOption :: IO Option
getOption = Option <$> get "DSN: " <*> get "UID: " <*> get "PWD: "

connect :: IO Connection
connect = do
    option <- getOption
    connectODBC $ show option

owner :: String
owner = unsafePerformIO $ get "OWNER: "

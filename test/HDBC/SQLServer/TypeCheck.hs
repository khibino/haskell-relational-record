module Main where

import Database.HDBC
import Database.HDBC.ODBC
import System.Environment
import System.IO

main :: IO ()
main = do
    args <- getArgs
    if length args > 1 then do
        conn <- connectODBC $ args !! 0
        rows <- quickQuery conn $ args !! 1
        mapM_ putStrLn $ map show rows
     else
        hPutStrLn stderr "query <DB file path> <SQL>"

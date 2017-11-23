module Main where

import Database.Relational (relationalQuery)
import Database.HDBC.Session (withConnectionIO, handleSqlError')
import Database.HDBC.Record.Query (runQuery)

import DataSource
import HrrDatatypeTest

main :: IO ()
main = handleSqlError' $ withConnectionIO connect $ \conn -> do
    runQuery conn (relationalQuery hrrDatatypeTest) () >>= print

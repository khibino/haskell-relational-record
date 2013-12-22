{-# LANGUAGE MonadComprehensions   #-}
module Main where

import Database.Relational.Query    ( query
                                    , relation
                                    , wheres
                                    , (.=.)
                                    , (!)
                                    , value
                                    , relationalQuery
                                    , Relation
                                    )
import Database.HDBC.Session        (withConnectionIO, handleSqlError')
import Database.HDBC.Record.Query   (runQuery)

import Data.Int                     (Int64)
import MySQLTestDataSource          (connect)
import User                         (user)
import qualified User as U

main :: IO ()
main = handleSqlError' $ withConnectionIO connect $ \conn -> do
    r <- runQuery conn (relationalQuery test) ()
    print r
    where
        test :: Relation () String
        test = relation
            [ u ! U.name'
            | u  <- query user
            , () <- wheres $ u ! U.id' .=. value (1 :: Int64)
            ]


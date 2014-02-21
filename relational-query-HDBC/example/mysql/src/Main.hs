{-# LANGUAGE MonadComprehensions   #-}
module Main where

import Database.Relational.Query    ( query
                                    , relation
                                    , wheres
                                    , (.=.)
                                    , (.>=.)
                                    , (!)
                                    , (><)
                                    , value
                                    , relationalQuery
                                    , Relation
                                    )
import Database.HDBC.Session        (withConnectionIO, handleSqlError')
import Database.HDBC.Record.Query   (runQuery)

import Data.Time                    (Day, fromGregorian)

import Example.DataSource           (connect)
import Example.User                 (user)
import qualified Example.User as U

main :: IO ()
main = handleSqlError' $ withConnectionIO connect $ \conn -> do
    printResults conn sample1
    printResults conn sample2
    where
        printResults c q = runQuery c (relationalQuery q) () >>= print

        completed = relation
            [ u
            | u  <- query user
            , () <- wheres $ u ! U.completed' .=. value 1
            ]

        sample1 :: Relation () (String, Day)
        sample1 = relation
            [ u ! U.name' >< u ! U.createdAt'
            | u  <- query completed
            ]

        sample2 :: Relation () (String, Day)
        sample2 = relation
            [ u ! U.email' >< u ! U.createdAt'
            | u  <- query completed
            , () <- wheres $ u ! U.createdAt' .>=. value (day 2014 2 10)
            ]

        day = fromGregorian


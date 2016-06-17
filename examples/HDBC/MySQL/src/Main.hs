{-# OPTIONS_GHC -fsimpl-tick-factor=200 #-}
{-# LANGUAGE MonadComprehensions, FlexibleContexts #-}
module Main where

import Database.Relational.Query    ( query
                                    , relation
                                    , aggregateRelation
                                    , wheres
                                    , (.=.)
                                    , (.>=.)
                                    , (!)
                                    , (><)
                                    , value
                                    , count
                                    , min'
                                    , groupBy
                                    , desc
                                    , id'
                                    , relationalQuery
                                    , Relation
                                    )
import Database.HDBC.Session        (withConnectionIO, handleSqlError')
import Database.HDBC.Record.Query   (runQuery')
import Database.HDBC                (runRaw, quickQuery', fromSql)

import Data.Time                    (Day, fromGregorian)
import Data.List                    (isInfixOf)
import Data.Int                     (Int8, Int64)

import Example.DataSource           (connect)
import Example.User                 (user)
import qualified Example.User as U

main :: IO ()
main = handleSqlError' $ withConnectionIO connect $ \conn -> do
    setSqlMode conn
    printResults conn sample1
    printResults conn sample2
    printResults conn sample3
    printResults conn sample4
    where
        printResults c q = runQuery' c (relationalQuery q) () >>= print

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

        sample3 :: Relation () (Int8, Int64)
        sample3 = aggregateRelation
            [ c >< count (u ! U.id')
            | u  <- query user
            , c  <- groupBy $ u ! U.completed'
            , () <- desc $ c ! id'
            ]

        sample4 :: Relation () (Maybe Day)
        sample4 = aggregateRelation
            [ min' (u ! U.createdAt')
            | u  <- query user
            ]

        setSqlMode conn = do
            mode <- quickQuery' conn "SELECT @@SESSION.sql_mode" []
            newmode <- case mode of
                [[sqlval]] ->
                    let val = fromSql sqlval in
                        if "IGNORE_SPACE" `isInfixOf` val
                            then return val
                            else return $ val ++ ",IGNORE_SPACE"
                _          ->
                    error "failed to get 'sql_mode'"
            runRaw conn $ "SET SESSION sql_mode = '" ++ newmode ++ "'"

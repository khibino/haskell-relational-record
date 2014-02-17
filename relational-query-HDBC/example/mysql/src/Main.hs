{-# LANGUAGE MonadComprehensions   #-}
module Main where

import Database.Relational.Query    ( query
                                    , relation
                                    , wheres
                                    , (.=.)
                                    , (!)
                                    , (><)
                                    , value
                                    , relationalQuery
                                    , Relation
                                    )
import Database.HDBC.Session        (withConnectionIO, handleSqlError')
import Database.HDBC.Record.Query   (runQuery)

import Data.Int                     (Int64)
import Data.Time                    (Day)

import Example.DataSource           (connect)
import Example.User                 (user)
import qualified Example.User as U

main :: IO ()
main = handleSqlError' $ withConnectionIO connect $ \conn -> do
    r <- runQuery conn (relationalQuery test) ()
    print r
    where
        test :: Relation () (String, Day)
        test = relation
            [ u ! U.name' >< u ! U.createdAt'
            | u  <- query user
            , () <- wheres $ u ! U.id' .=. value (1 :: Int64)
            ]


{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MonadComprehensions   #-}

module Main where

import Data.Int (Int64)
import Prelude hiding (id)

import Database.Relational.Query  ( query
                                  , relation
                                  , wheres
                                  , (.=.)
                                  , (!)
                                  , value
                                  , relationalQuery
                                  , Relation
                                  )
import Database.HDBC.Session      ( withConnectionIO
                                  , handleSqlError'
                                  )
import Database.HDBC.Record.Query ( runQuery
                                  )
import MySQLTestDataSource        (connect, defineTable)

$(defineTable
    []
    "test" "user" [])

main :: IO ()
main = handleSqlError' $ withConnectionIO connect $ \conn -> do
    r <- runQuery conn (relationalQuery test) ()
    print r
    where
        test :: Relation () String
        test = relation
            [ u ! name'
            | u <- query user
            , () <- wheres $ u ! id' .=. value (1 :: Int64)
            ]


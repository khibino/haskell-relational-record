{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Test.Hspec

import Language.Haskell.TH          (runQ)
import Data.Int                     (Int32)
import Data.Time                    (LocalTime)
import Database.HDBC.Session        (withConnectionIO)
import Database.HDBC.Record.Query   (runQuery')
import Database.Record.TH           (derivingShow)
import Database.Relational.Query    ( query
                                    , relation
                                    , wheres
                                    , (.=.)
                                    , (!)
                                    , value
                                    , relationalQuery
                                    )
import Database.HDBC.Schema.Driver  (getPrimaryKey, getFieldsWithMap)
import Database.HDBC.Schema.MySQL   (driverMySQL)

import Prelude hiding (id)
import qualified DB.Source as DB

-- TODO: get and define
$(DB.defineTable
    []
    "TEST" "user" [derivingShow])

main :: IO ()
main = hspec $ do
    describe "getPrimaryKey" $ do
        it "returns one primary key" $ do
            keys <- withConnectionIO DB.connect $ \c -> getPrimaryKey driverMySQL c "TEST" "test_pk1"
            keys `shouldBe` ["a"]
        it "returns two primary keys" $ do
            keys <- withConnectionIO DB.connect $ \c -> getPrimaryKey driverMySQL c "TEST" "test_pk2"
            keys `shouldBe` ["a", "b"]

    describe "getFieldsWithMap" $ do
        it "returns 'NOT NULL' column positions" $ do
            ( _, ps) <- withConnectionIO DB.connect $ \c -> getFieldsWithMap driverMySQL [] c "TEST" "test_nn1"
            ps `shouldBe` [0, 2, 4]
        it "returns column types" $ do
            (tm,  _) <- withConnectionIO DB.connect $ \c -> getFieldsWithMap driverMySQL [] c "TEST" "test_nn1"
            types  <- mapM (runQ . snd) tm
            expect <- mapM runQ [[t|Int32|], [t|Maybe Int32|], [t|String|], [t|Maybe String|], [t|LocalTime|]]
            types  `shouldBe` expect

    describe "basic tests" $
        it "returns data types" $ do
            decs <- runQ $ DB.defineTable [] "TEST" "user" []
            decs `shouldSatisfy` not . null

    describe "run query" $
        it "returns some records" $ do
            let test1 = relation $ do
                    u  <- query user
                    wheres $ u ! id' .=. value 1
                    return (u ! name')
            rs <- runDB test1
            rs `shouldSatisfy` not . null

  where
    runDB q =
        withConnectionIO DB.connect $ \conn ->
            runQuery' conn (relationalQuery q) ()


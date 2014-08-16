{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Test.Hspec

import Prelude hiding (id)
import Language.Haskell.TH          (runQ)

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
import qualified DB.Source as DB

-- TODO: get and define
$(DB.defineTable
    []
    "TEST" "user" [derivingShow])

main :: IO ()
main = hspec $ do
    describe "basic tests" $
        it "should generate data types" $ do
            decs <- runQ $ DB.defineTable [] "TEST" "user" []
            decs `shouldSatisfy` not . null

    describe "run query" $
        it "should get a record" $ do
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


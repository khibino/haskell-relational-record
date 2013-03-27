{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PgTest (
  tests
  ) where

import Distribution.TestSuite (Test)
import PgTestDataSource (defineTable)
-- import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)
-- import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
-- import Database.HDBC.Schema.Driver (typeMap)
-- import Database.HDBC.TH (defineTableFromDB)
import Data.ByteString (ByteString)
import Data.Text (Text)

tests :: [Test]
tests =  []

$(defineTable
  [("varchar", [t| ByteString |]),
   ("text", [t| Text |])
  ]
  "TEST" "test_table0" [])
-- defineTableFromDB
--   (connectPostgreSQL "dbname=testdb")
--   (driverPostgreSQL { typeMap = [("varchar", [t| Text |])] })
--   "TEST" "test_table0" []

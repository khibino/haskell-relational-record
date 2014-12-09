{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SQLite3Test where

import Data.Time (UTCTime)
import Distribution.TestSuite (Test)
import SQLite3TestDataSource (defineTable)

tests :: IO [Test]
tests = return []

$(defineTable
  [("date", [t| UTCTime |]),
   ("smallint", [t| Int |]),
   ("varchar", [t| String |])
  ]
  "main" "test_table0" [])

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PgTest (
  tests
  ) where

import Distribution.TestSuite (Test)
import PgTestDataSource (defineTable)

tests :: [Test]
tests =  []

$(defineTable "TEST" "test_table0" [])

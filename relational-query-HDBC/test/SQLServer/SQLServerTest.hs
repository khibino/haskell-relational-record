{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SQLServerTest where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Distribution.TestSuite (Test)
import SQLServerTestDataSource (defineTable)

tests :: IO [Test]
tests = return []

$(defineTable
  [("varchar", [t| ByteString |]),
   ("text", [t| Text |])
  ]
  "TEST" "test_table0" [])

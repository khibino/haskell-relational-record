{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module KeyTest where

import PgTestDataSource (defineTable)
import Database.HDBC.Query.TH (makeRecordPersistableDefault)

$(defineTable []
  "EXAMPLE2" "keyTest" [])


data Foo = Foo Int String
data Bar = Bar { x :: Int, y :: String }

$(makeRecordPersistableDefault ''Foo)
$(makeRecordPersistableDefault ''Bar)

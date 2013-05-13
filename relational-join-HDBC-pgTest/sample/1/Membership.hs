{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

module Membership where

import PgTestDataSource (defineTable)

$(defineTable []
  "SAMPLE1" "membership" [])


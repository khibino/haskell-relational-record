{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

module Membership where

import PgTestDataSource (defineTable)

$(defineTable []
  "EXAMPLE1" "membership" [])


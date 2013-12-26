{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Membership where

import PgTestDataSource (defineTable)

$(defineTable []
  "EXAMPLE1" "membership" [])

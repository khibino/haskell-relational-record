{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module Membership where

import GHC.Generics (Generic)
import PgTestDataSource (defineTable)

$(defineTable []
  "EXAMPLE1" "membership" [''Generic])

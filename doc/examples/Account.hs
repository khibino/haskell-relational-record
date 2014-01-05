{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Account where

import Prelude hiding (id)
import Database.Record.TH (derivingShow)

import DataSource (defineTable)

$(defineTable []
  "LEARNINGSQL" "account" [derivingShow])

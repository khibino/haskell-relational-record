{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

module Branch where

import Prelude hiding (id, zip)
import Database.Record.TH (derivingShow)

import DataSource (defineTable)

$(defineTable []
  "LEARNINGSQL" "branch" [derivingShow])

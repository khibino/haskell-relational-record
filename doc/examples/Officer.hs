{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

module Officer where

import Prelude hiding (id)
import Database.Record.TH (derivingShow)

import DataSource (defineTable)

$(defineTable []
  "LEARNINGSQL" "officer" [derivingShow])

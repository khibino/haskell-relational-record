{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

module Customer where

import Prelude hiding (id)
import Database.Record.TH (derivingShow)

import DataSource (defineTable)

$(defineTable []
  "LEARNINGSQL" "customer" [derivingShow])

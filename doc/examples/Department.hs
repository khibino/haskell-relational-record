{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

module Department where

import Prelude hiding (id)
import Database.Record.TH (derivingShow)

import DataSource (defineTable)

$(defineTable []
  "LEARNINGSQL" "department" [derivingShow])

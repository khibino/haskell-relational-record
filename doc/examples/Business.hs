{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

module Business where

import Prelude hiding (id)
import Database.Record.TH (derivingShow)

import DataSource (defineTable)

$(defineTable []
  "LEARNINGSQL" "business" [derivingShow])

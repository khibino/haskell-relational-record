{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

module Employee where

import Prelude hiding (id)
import Database.Record.TH (derivingShow)

import DataSource (defineTable)

$(defineTable []
  "LEARNINGSQL" "employee" [derivingShow])

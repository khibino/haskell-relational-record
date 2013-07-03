{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

module Transaction where

import Prelude hiding (id)
import Database.Record.TH (derivingShow)

import DataSource (defineTable)

$(defineTable []
  "LEARNINGSQL" "transaction" [derivingShow])

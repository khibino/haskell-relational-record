{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

module Individual where

import Prelude hiding (id)
import Database.Record.TH (derivingShow)

import DataSource (defineTable)

$(defineTable []
  "LEARNINGSQL" "individual" [derivingShow])

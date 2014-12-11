{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Officer where

import Prelude hiding (id)
import Database.Record.TH (derivingShow)

import DataSource (convTypes, defineTable)

$(defineTable convTypes
  "main" "officer" [derivingShow])

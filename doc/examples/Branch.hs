{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Branch where

import Prelude hiding (id, zip)
import Database.Record.TH (derivingShow)

import DataSource (convTypes, defineTable)

$(defineTable convTypes
  "main" "branch" [derivingShow])

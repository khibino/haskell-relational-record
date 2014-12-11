{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Customer where

import Prelude hiding (id)
import Database.Record.TH (derivingShow)

import DataSource (convTypes, defineTable)

$(defineTable convTypes
  "main" "customer" [derivingShow])

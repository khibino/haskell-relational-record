{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Product where

import Prelude hiding (id, product)
import Database.Record.TH (derivingShow)

import DataSource (defineTable)

$(defineTable []
  "LEARNINGSQL" "product" [derivingShow])

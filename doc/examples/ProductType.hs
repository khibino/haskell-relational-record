{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module ProductType where

import Prelude hiding (id)
import Database.Record.TH (derivingShow)

import DataSource (convTypes, defineTable)

$(defineTable convTypes
  "main" "product_type" [derivingShow])

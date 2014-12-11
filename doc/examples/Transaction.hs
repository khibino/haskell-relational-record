{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Transaction where

import Prelude hiding (id)
import Database.Record.TH (derivingShow)

import DataSource (convTypes, defineTable)

$(defineTable convTypes
  "main" "transaction0" [derivingShow])

type Transaction = Transaction0

transaction = transaction0

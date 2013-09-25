{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

module User where

import Prelude hiding (id)
import PgTestDataSource (defineTable)
import Database.Record.TH (derivingShow)

$(defineTable []
  "EXAMPLE1" "user" [derivingShow])

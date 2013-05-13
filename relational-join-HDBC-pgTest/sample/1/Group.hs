{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

module Group where

import Prelude hiding (id)
import PgTestDataSource (defineTable)
import Database.Record.TH (derivingShow)

$(defineTable []
  "SAMPLE1" "group" [derivingShow])


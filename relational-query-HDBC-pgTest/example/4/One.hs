{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module One where

import Prelude hiding (seq)
import PgTestDataSource (defineTable)
import Database.Record.TH (derivingShow)

$(defineTable []
  "EXAMPLE4" "one" [derivingShow])

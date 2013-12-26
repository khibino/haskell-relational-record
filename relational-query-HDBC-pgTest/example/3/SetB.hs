{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module SetB where

import Prelude hiding (seq)
import PgTestDataSource (defineTable)
import Database.Record.TH (derivingShow)

$(defineTable []
  "EXAMPLE3" "set_b" [derivingShow])

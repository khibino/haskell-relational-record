{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module User where

import Prelude hiding (id)
import PgTestDataSource (defineTable)
import Database.Record.TH (derivingShow)

$(defineTable []
  "EXAMPLE1" "user" [derivingShow])

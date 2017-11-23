{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DataKinds, DeriveGeneric #-}

module History where

import GHC.Generics (Generic)
import Prelude hiding (seq, log)
import PgTestDataSource (defineTable)

$(defineTable []
  "EXAMPLE3" "history" [''Show, ''Generic])

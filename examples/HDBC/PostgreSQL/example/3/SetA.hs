{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module SetA where

import GHC.Generics (Generic)
import Prelude hiding (seq)
import PgTestDataSource (defineTable)

$(defineTable []
  "EXAMPLE3" "set_a" [''Show, ''Generic])

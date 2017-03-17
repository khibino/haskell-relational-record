{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module SetB where

import GHC.Generics (Generic)
import Prelude hiding (seq)
import PgTestDataSource (defineTable)

$(defineTable []
  "EXAMPLE3" "set_b" [''Show, ''Generic])

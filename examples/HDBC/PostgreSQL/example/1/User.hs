{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module User where

import GHC.Generics (Generic)
import Prelude hiding (id)
import PgTestDataSource (defineTable)

$(defineTable []
  "EXAMPLE1" "user" [''Show, ''Generic])

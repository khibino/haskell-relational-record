{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module Group where

import GHC.Generics (Generic)
import Prelude hiding (id)
import PgTestDataSource (defineTable)

$(defineTable []
  "EXAMPLE1" "group" [''Show, ''Generic])

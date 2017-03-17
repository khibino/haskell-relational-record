{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module KeyTest where

import GHC.Generics (Generic)
import PgTestDataSource (defineTable)

$(defineTable []
  "EXAMPLE2" "keyTest" [''Generic])

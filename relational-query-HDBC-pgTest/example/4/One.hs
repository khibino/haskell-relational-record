{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module One where

import Prelude hiding (seq)
import PgTestDataSource (defineTable)

$(defineTable []
  "EXAMPLE4" "one" [''Show])

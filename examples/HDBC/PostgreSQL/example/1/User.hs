{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module User where

import Prelude hiding (id)
import PgTestDataSource (defineTable)

$(defineTable []
  "EXAMPLE1" "user" [''Show])

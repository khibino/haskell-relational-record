{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Group where

import Prelude hiding (id)
import PgTestDataSource (defineTable)

$(defineTable []
  "EXAMPLE1" "group" [''Show])

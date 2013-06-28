{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module KeyTest where

import PgTestDataSource (defineTable)

$(defineTable []
  "EXAMPLE2" "keyTest" [])

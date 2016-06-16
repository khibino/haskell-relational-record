{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module ProductType where

import Database.Record.TH.SQLite3 (defineTable)

$(defineTable "examples.db" "product_type")

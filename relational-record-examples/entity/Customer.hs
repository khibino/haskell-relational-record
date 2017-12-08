{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric, DataKinds #-}

module Customer where

import Database.Record.TH.SQLite3 (defineTable)

$(defineTable "examples.db" "customer")

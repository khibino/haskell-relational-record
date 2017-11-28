{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric, DataKinds #-}

module Department where

import Database.Record.TH.SQLite3 (defineTable)

$(defineTable "examples.db" "department")

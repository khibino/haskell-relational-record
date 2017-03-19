{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module Department where

import Database.Record.TH.SQLite3 (defineTable)

$(defineTable "examples.db" "department")

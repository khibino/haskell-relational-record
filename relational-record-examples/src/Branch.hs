{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric, DataKinds #-}

module Branch where

import Database.Record.TH.SQLite3 (defineTable)

$(defineTable "examples.db" "branch")

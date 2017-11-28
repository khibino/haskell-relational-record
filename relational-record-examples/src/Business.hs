{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric, DataKinds #-}

module Business where

import Database.Record.TH.SQLite3 (defineTable)

$(defineTable "examples.db" "business")

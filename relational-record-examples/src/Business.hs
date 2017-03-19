{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module Business where

import Database.Record.TH.SQLite3 (defineTable)

$(defineTable "examples.db" "business")

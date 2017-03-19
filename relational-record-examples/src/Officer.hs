{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module Officer where

import Database.Record.TH.SQLite3 (defineTable)

$(defineTable "examples.db" "officer")

{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric, DataKinds #-}

module Account where

import Database.Record.TH.SQLite3 (defineTable)

$(defineTable "examples.db" "account")

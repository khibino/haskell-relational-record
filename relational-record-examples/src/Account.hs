{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Account where

import Database.Record.TH.SQLite3 (defineTable)

$(defineTable "examples.db" "account")

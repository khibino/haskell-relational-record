{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Officer where

import Database.Record.TH.SQLite3 (defineTable)

$(defineTable "examples.db" "officer")

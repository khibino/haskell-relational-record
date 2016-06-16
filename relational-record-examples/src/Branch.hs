{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Branch where

import Database.Record.TH.SQLite3 (defineTable)

$(defineTable "examples.db" "branch")

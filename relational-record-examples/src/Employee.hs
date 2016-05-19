{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Employee where

import Database.Record.TH.SQLite3 (defineTable)

$(defineTable "examples.db" "employee")

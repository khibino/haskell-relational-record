{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Business where

import Database.Record.TH.SQLite3 (defineTable)

$(defineTable "examples.db" "business")

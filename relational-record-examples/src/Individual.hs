{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Individual where

import Database.Record.TH.SQLite3 (defineTable)

$(defineTable "examples.db" "individual")

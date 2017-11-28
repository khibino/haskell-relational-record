{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric, DataKinds #-}

module Individual where

import Database.Record.TH.SQLite3 (defineTable)

$(defineTable "examples.db" "individual")

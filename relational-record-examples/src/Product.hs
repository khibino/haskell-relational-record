{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric, DataKinds #-}

module Product where

import Database.Record.TH.SQLite3 (defineTable)
import Prelude hiding (product)

$(defineTable "examples.db" "product")

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Relational.Schema.SQLite3Syscat.IndexList where

import Data.Int (Int64)
import Database.Record.TH (derivingShow)
import Database.Relational.Query.TH (defineTableTypesAndRecordDefault)

import Database.Relational.Schema.SQLite3Syscat.Config (config)


$(defineTableTypesAndRecordDefault config
  "pragma" "index_list"
  [
-- pragma "main.index_list"
-- column                type                NULL
-- --------------------- ------------------- ------
-- seq                   integer             No
    ("seq", [t|Int64|]),
-- name                  text                No
    ("name", [t|String|]),
-- unique                integer             No
    ("unique", [t|Int64|])
  ]
  [derivingShow])

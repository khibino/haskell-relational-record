{-# LANGUAGE TemplateHaskell #-}

module Database.Relational.Schema.SQLite3Syscat.IndexInfo where

import Data.Int (Int64)
import Database.Record.TH (derivingShow)
import Database.Relational.Query.TH (defineTableTypesAndRecordDefault)

$(defineTableTypesAndRecordDefault
  "pragma" "index_info"
  [
-- pragma "index_info"
-- column                type                NULL
-- --------------------- ------------------- ------
-- seqno                 integer             No
    ("seqno", [t|Int64|]),
-- cid                   integer             No
    ("cid", [t|Int64|]),
-- name                  text                No
    ("name", [t|String|])
  ]
  [derivingShow])

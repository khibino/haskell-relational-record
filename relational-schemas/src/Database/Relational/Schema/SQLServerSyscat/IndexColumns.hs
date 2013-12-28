{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Relational.Schema.SQLServerSyscat.IndexColumns where

import Data.Int (Int32)
import Database.Record.TH (derivingShow)
import Database.Relational.Query.TH (defineTableTypesAndRecordDefault)

$(defineTableTypesAndRecordDefault
  "sys" "index_columns"
  [
  -- column                schema  type                length   NULL
  -- --------------------- ------- ------------------- -------- ------
  -- object_id             sys     int                        4 No
    ("object_id", [t|Int32|]),
  -- index_id              sys     int                        4 No
    ("index_id", [t|Int32|]),
  -- index_column_id       sys     int                        4 No
    ("column_id", [t|Int32|]),
  -- key_ordinal           sys     tinyint                    1 No
    ("key_ordinal", [t|Int32|]),
  -- partition_ordinal     sys     tinyint                    1 No
    --("partition_ordinal", [t|Int32|]),
  -- is_descending_key     sys     bit                        1 No
    --("is_descending_key", [t|Bool|]),
  -- is_included_column    sys     bit                        1 No
    ("is_included_column", [t|Bool|])
  ]
  [derivingShow])

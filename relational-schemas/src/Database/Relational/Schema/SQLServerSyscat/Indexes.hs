{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Relational.Schema.SQLServerSyscat.Indexes where

--import Data.ByteString (ByteString)
import Data.Int (Int32)
import Database.Relational.Query.TH (defineTableTypesAndRecordDefault)

import Database.Relational.Schema.SQLServerSyscat.Config (config)


$(defineTableTypesAndRecordDefault config
  "sys" "indexes"
  [
-- View "sys.indexes"
-- column                schema  type                length   NULL
-- --------------------- ------- ------------------- -------- ------
-- object_id             sys     int                        4 No
    ("object_id", [t|Int32|]),
-- name                  sys     sysname(nvarchar)        128 Yes
    --("name", [t|Maybe String|]),
-- index_id              sys     int                        4 No
    ("index_id", [t|Int32|]),
-- type                  sys     tinyint                    1 No
    --("type", [t|Char|]),
-- type_desc             sys     nvarchar                  60 Yes
    --("type_desc", [t|Maybe String|]),
-- is_unique             sys     bit                        1 Yes
    --("is_unique", [t|Maybe Bool|]),
-- data_space_id         sys     int                        4 No
    --("data_space_id", [t|Int32|]),
-- ignore_dup_key        sys     bit                        1 Yes
    --("ignore_dup_key", [t|Maybe Bool|]),
-- is_primary_key        sys     bit                        1 Yes
    ("is_primary_key", [t|Maybe Bool|])--,
-- is_unique_constraint  sys     bit                        1 Yes
    --("is_unique_constraint", [t|Maybe Bool|]),
-- fill_factor           sys     tinyint                    1 No
    --("fill_factor", [t|Int32|]),
-- is_padded             sys     bit                        1 Yes
    --("is_padded", [t|Maybe Bool|]),
-- is_disabled           sys     bit                        1 Yes
    --("is_disabled", [t|Maybe Bool|]),
-- is_hypothetical       sys     bit                        1 Yes
    --("is_hypothetical", [t|Maybe Bool|]),
-- allow_row_locks       sys     bit                        1 Yes
    --("allow_row_locks", [t|Maybe Bool|]),
-- allow_page_locks      sys     bit                        1 Yes
    --("allow_page_locks", [t|Maybe Bool|]),
-- has_filter            sys     bit                        1 Yes
    --("has_filter", [t|Maybe Bool|]),
-- filter_definition     sys     nvarchar                 max Yes
    --("filter_definition", [t|Maybe ByteString|])
  ]
  [''Show])

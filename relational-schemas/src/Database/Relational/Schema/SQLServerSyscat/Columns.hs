{-# LANGUAGE TemplateHaskell #-}

module Database.Relational.Schema.SQLServerSyscat.Columns where

import Data.Int (Int32)
import Database.Record.TH (derivingShow)
import Database.Relational.Query.TH (defineTableTypesAndRecordDefault)

$(defineTableTypesAndRecordDefault
  "sys" "columns"
  [
  -- column                schema  type                length   NULL
  -- --------------------- ------- ------------------- -------- ------
  -- object_id             sys     int                        4 No
    ("object_id", [t|Int32|]),
  -- name                  sys     sysname(nvarchar)        128 Yes
    ("name", [t|Maybe String|]),
  -- column_id             sys     int                        4 No
    ("column_id", [t|Int32|]),
  -- system_type_id        sys     tinyint                    1 No
    --("system_type_id", [t|Int32|]),
  -- user_type_id          sys     int                        4 No
    ("user_type_id", [t|Int32|]),
  -- max_length            sys     smallint                   2 No
    --("max_length", [t|Int32|]),
  -- precision             sys     tinyint                    1 No
    --("precision", [t|Int32|]),
  -- scale                 sys     tinyint                    1 No
    --("scale", [t|Int32|]),
  -- collation_name        sys     sysname(nvarchar)        128 Yes
    --("collation_name", [t|Maybe String|]),
  -- is_nullable           sys     bit                        1 Yes
    ("is_nullable", [t|Maybe Bool|])--,
  -- is_ansi_padded        sys     bit                        1 No
    --("is_ansi_padded", [t|Bool|]),
  -- is_rowguidcol         sys     bit                        1 No
    --("is_rowguidcol", [t|Bool|]),
  -- is_identity           sys     bit                        1 No
    --("is_identity", [t|Bool|]),
  -- is_computed           sys     bit                        1 No
    --("is_computed", [t|Bool|]),
  -- is_filestream         sys     bit                        1 No
    --("is_filestream", [t|Bool|]),
  -- is_replicated         sys     bit                        1 Yes
    --("is_replicated", [t|Maybe Bool|]),
  -- is_non_sql_subscribed sys     bit                        1 Yes
    --("is_non_sql_subscribed", [t|Maybe Bool|]),
  -- is_merge_published    sys     bit                        1 Yes
    --("is_merge_published", [t|Maybe Bool|]),
  -- is_dts_repllicated    sys     bit                        1 Yes
    --("is_dts_replicated", [t|Maybe Bool|]),
  -- is_xml_document       sys     bit                        1 No
    --("is_xml_document", [t|Bool|]),
  -- xml_collection_id     sys     int                        4 No
    --("xml_collection_id", [t|Int32|]),
  -- default_object_id     sys     int                        4 No
    --("default_object_id", [t|Int32|]),
  -- rule_object_id        sys     int                        4 No
    --("rule_object_id", [t|Int32|]),
  -- is_sparse             sys     bit                        1 Yes
    --("is_sparse", [t|Maybe Bool|]),
  -- is_column_set         sys     bit                        1 Yes
    --("is_column_set", [t|Maybe Bool|])
  ]
  [derivingShow])

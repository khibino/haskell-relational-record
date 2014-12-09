{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Relational.Schema.SQLServerSyscat.Types where

import Data.Int (Int32)
import Database.Record.TH (derivingShow)
import Database.Relational.Query.TH (defineTableTypesAndRecordDefault)

$(defineTableTypesAndRecordDefault
  "sys" "types"
  [
-- View "sys.types"
-- column                schema  type                length   NULL
-- --------------------- ------- ------------------- -------- ------
-- name                  sys     sysname(nvarchar)        128 No
    ("name", [t|String|]),
-- system_type_id        sys     tinyint                    1 No
    --("system_type_id", [t|Int32|]),
-- user_type_id          sys     int                        4 No
    ("user_type_id", [t|Int32|]),
-- schema_id             sys     int                        4 No
    ("schema_id", [t|Int32|])--,
-- principal_id          sys     int                        4 Yes
    --("principal_id", [t|Maybe Int32|]),
-- max_length            sys     int                        4 No
    --("max_length", [t|Int32|]),
-- precision             sys     tinyint                    1 No
    --("precision", [t|Int32|]),
-- scale                 sys     tinyint                    1 No
    --("scale", [t|Int32|]),
-- collation_name        sys     sysname(nvarchar)        128 Yes
    --("collation_name", [t|Maybe String|]),
-- is_nullable           sys     bit                        1 Yes
    --("is_nullable", [t|Maybe Bool|]),
-- is_user_defined       sys     bit                        1 No
    --("is_user_defined", [t|Bool|]),
-- is_assembly_type      sys     bit                        1 No
    --("is_assembly_type", [t|Bool|]),
-- default_object_id     sys     int                        4 No
    --("default_object_id", [t|Int32|]),
-- rule_object_id        sys     int                        4 No
    --("rule_object_id", [t|Int32|]),
-- is_table_type         sys     bit                        1 No
    --("is_table_type", [t|Bool|])
  ]
  [derivingShow])

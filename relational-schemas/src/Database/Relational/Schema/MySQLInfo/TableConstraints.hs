{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Relational.Schema.MySQLInfo.TableConstraints where

import Database.Record.TH           (derivingShow)
import Database.Relational.Query.TH (defineTableTypesAndRecordDefault)

$(defineTableTypesAndRecordDefault
    "INFORMATION_SCHEMA" "table_constraints"
    [ ("table_schema"       , [t| String |])
    , ("table_name"         , [t| String |])
    , ("constraint_name"    , [t| String |])
    , ("constraint_type"    , [t| String |])
    ]
    [derivingShow])

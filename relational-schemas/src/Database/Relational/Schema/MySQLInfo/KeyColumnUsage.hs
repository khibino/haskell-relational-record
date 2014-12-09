{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Relational.Schema.MySQLInfo.KeyColumnUsage where

import Data.Int                     (Int16)
import Database.Record.TH           (derivingShow)
import Database.Relational.Query.TH (defineTableTypesAndRecordDefault)

$(defineTableTypesAndRecordDefault
    "INFORMATION_SCHEMA" "key_column_usage"
    [ ("constraint_name"    , [t| String |])
    , ("table_schema"       , [t| String |])
    , ("table_name"         , [t| String |])
    , ("column_name"        , [t| String |])
    , ("ordinal_position"   , [t| Int16 |])
    ]
    [derivingShow])

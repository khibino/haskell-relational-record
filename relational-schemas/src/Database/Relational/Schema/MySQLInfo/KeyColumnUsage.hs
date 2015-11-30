{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Relational.Schema.MySQLInfo.KeyColumnUsage where

import Data.Int                     (Int16)
import Database.Relational.Query.TH (defineTableTypesAndRecordDefault)

import Database.Relational.Schema.MySQLInfo.Config (config)


$(defineTableTypesAndRecordDefault config
    "INFORMATION_SCHEMA" "key_column_usage"
    [ ("constraint_name"    , [t| String |])
    , ("table_schema"       , [t| String |])
    , ("table_name"         , [t| String |])
    , ("column_name"        , [t| String |])
    , ("ordinal_position"   , [t| Int16 |])
    ]
    [''Show])

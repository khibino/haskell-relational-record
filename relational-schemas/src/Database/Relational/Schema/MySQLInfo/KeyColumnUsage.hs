{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.Relational.Schema.MySQLInfo.KeyColumnUsage where

import GHC.Generics (Generic)
import Data.Int                     (Int16)
import Database.Relational.Query.TH (defineTableTypesAndRecord)

import Database.Relational.Schema.MySQLInfo.Config (config)


$(defineTableTypesAndRecord config
    "INFORMATION_SCHEMA" "key_column_usage"
    [ ("constraint_name"    , [t| String |])
    , ("table_schema"       , [t| String |])
    , ("table_name"         , [t| String |])
    , ("column_name"        , [t| String |])
    , ("ordinal_position"   , [t| Int16 |])
    ]
    [''Show, ''Generic])

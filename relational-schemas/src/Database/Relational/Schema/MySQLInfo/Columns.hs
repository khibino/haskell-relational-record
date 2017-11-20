{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.Relational.Schema.MySQLInfo.Columns where

import GHC.Generics (Generic)
import Data.Int                     (Int16)
import Database.Relational.TH (defineTableTypesAndRecord)

import Database.Relational.Schema.MySQLInfo.Config (config)


$(defineTableTypesAndRecord config
    "INFORMATION_SCHEMA" "columns"
    [ ("table_schema",      [t|String|])
    , ("table_name",        [t|String|])
    , ("column_name",       [t|String|])
    , ("ordinal_position",  [t|Int16|])
    , ("column_default",    [t|Maybe String|])
    , ("is_nullable",       [t|String|])
    , ("data_type",         [t|String|])
    ]
    [''Show, ''Generic])

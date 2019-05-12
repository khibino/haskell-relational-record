{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.Relational.Schema.MySQL.TableConstraints where

import GHC.Generics (Generic)
import Database.Relational.TH (defineTableTypesAndRecord)

import Database.Relational.Schema.MySQL.Config (config)


$(defineTableTypesAndRecord config
    "INFORMATION_SCHEMA" "table_constraints"
    [ ("table_schema"       , [t| String |])
    , ("table_name"         , [t| String |])
    , ("constraint_name"    , [t| String |])
    , ("constraint_type"    , [t| String |])
    ]
    [''Show, ''Generic])

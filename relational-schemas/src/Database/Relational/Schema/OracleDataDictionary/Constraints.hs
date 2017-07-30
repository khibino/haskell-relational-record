{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.Relational.Schema.OracleDataDictionary.Constraints where

import GHC.Generics (Generic)
import Database.Relational.TH (defineTableTypesAndRecord)

import Database.Relational.Schema.OracleDataDictionary.Config (config)


$(defineTableTypesAndRecord config
    "SYS" "dba_constraints"
    -- Column                                    NULL?    Datatype
    -- ----------------------------------------- -------- ----------------------------
    -- OWNER                                              VARCHAR2(30)
    [ ("owner", [t|Maybe String|])
    -- CONSTRAINT_NAME                           NOT NULL VARCHAR2(30)
    , ("constraint_name", [t|String|])
    -- CONSTRAINT_TYPE                                    VARCHAR2(1)
    , ("constraint_type", [t|Maybe String|])
    -- TABLE_NAME                                NOT NULL VARCHAR2(30)
    , ("table_name", [t|String|])
    -- SEARCH_CONDITION                                   LONG
    -- R_OWNER                                            VARCHAR2(30)
    -- R_CONSTRAINT_NAME                                  VARCHAR2(30)
    -- DELETE_RULE                                        VARCHAR2(9)
    -- STATUS                                             VARCHAR2(8)
    -- DEFERRABLE                                         VARCHAR2(14)
    -- DEFERRED                                           VARCHAR2(9)
    -- VALIDATED                                          VARCHAR2(13)
    -- GENERATED                                          VARCHAR2(14)
    -- BAD                                                VARCHAR2(3)
    -- RELY                                               VARCHAR2(4)
    -- LAST_CHANGE                                        DATE
    -- INDEX_OWNER                                        VARCHAR2(30)
    -- INDEX_NAME                                         VARCHAR2(30)
    -- INVALID                                            VARCHAR2(7)
    -- VIEW_RELATED                                       VARCHAR2(14)
    ] [''Show, ''Generic])

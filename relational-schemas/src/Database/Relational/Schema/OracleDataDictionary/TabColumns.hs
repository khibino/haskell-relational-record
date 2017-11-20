{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.Relational.Schema.OracleDataDictionary.TabColumns where

import GHC.Generics (Generic)
import Data.Int (Int32)
import Database.Relational.TH (defineTableTypesAndRecord)

import Database.Relational.Schema.OracleDataDictionary.Config (config)


$(defineTableTypesAndRecord config
    "SYS" "dba_tab_columns"
    -- Column                                    NULL?    Datatype
    -- ----------------------------------------- -------- ----------------------------
    -- OWNER                                     NOT NULL VARCHAR2(30)
    [ ("owner", [t|String|])
    -- TABLE_NAME                                NOT NULL VARCHAR2(30)
    , ("table_name", [t|String|])
    -- COLUMN_NAME                               NOT NULL VARCHAR2(30)
    , ("column_name", [t|String|])
    -- DATA_TYPE                                          VARCHAR2(106)
    , ("data_type", [t|Maybe String|])
    -- DATA_TYPE_MOD                                      VARCHAR2(3)
    , ("data_type_mod", [t|Maybe String|])
    -- DATA_TYPE_OWNER                                    VARCHAR2(30)
    , ("data_type_owner", [t|Maybe String|])
    -- DATA_LENGTH                               NOT NULL NUMBER
    , ("data_length", [t|Int32|])
    -- DATA_PRECISION                                     NUMBER
    , ("data_precision", [t|Maybe Int32|])
    -- DATA_SCALE                                         NUMBER
    , ("data_scale", [t|Maybe Int32|])
    -- NULLABLE                                           VARCHAR2(1)
    , ("nullable", [t|Maybe String|])
    -- COLUMN_ID                                          NUMBER
    , ("column_id", [t|Maybe Int32|])
    -- DEFAULT_LENGTH                                     NUMBER
    , ("default_length", [t|Maybe Int32|])
    -- DATA_DEFAULT                                       LONG
    , ("data_default", [t|Maybe String|])
    -- NUM_DISTINCT                                       NUMBER
    , ("num_distinct", [t|Maybe Int32|])
    -- LOW_VALUE                                          RAW(32)
    -- , ("low_value", [t|Maybe ByteString|])
    -- HIGH_VALUE                                         RAW(32)
    -- , ("high_value", [t|Maybe ByteString|])
    -- DENSITY                                            NUMBER
    , ("density", [t|Maybe Int32|])
    -- NUM_NULLS                                          NUMBER
    , ("num_nulls", [t|Maybe Int32|])
    -- NUM_BUCKETS                                        NUMBER
    , ("num_buckets", [t|Maybe Int32|])
    -- LAST_ANALYZED                                      DATE
    -- , ("last_analyzed", [t|Maybe Day|])
    -- SAMPLE_SIZE                                        NUMBER
    , ("sample_size", [t|Maybe Int32|])
    -- CHARACTER_SET_NAME                                 VARCHAR2(44)
    , ("character_set_name", [t|Maybe String|])
    -- CHAR_COL_DECL_LENGTH                               NUMBER
    , ("char_col_decl_length", [t|Maybe Int32|])
    -- GLOBAL_STATS                                       VARCHAR2(3)
    , ("global_stats", [t|Maybe String|])
    -- USER_STATS                                         VARCHAR2(3)
    , ("user_stats", [t|Maybe String|])
    -- AVG_COL_LEN                                        NUMBER
    , ("avg_col_len", [t|Maybe Int32|])
    -- CHAR_LENGTH                                        NUMBER
    , ("char_length", [t|Maybe Int32|])
    -- CHAR_USED                                          VARCHAR2(1)
    , ("char_used", [t|Maybe String|])
    -- V80_FMT_IMAGE                                      VARCHAR2(3)
    , ("v80_fmt_image", [t|Maybe String|])
    -- DATA_UPGRADED                                      VARCHAR2(3)
    , ("data_upgraded", [t|Maybe String|])
    -- HISTOGRAM                                          VARCHAR2(15)
    , ("histogram", [t|Maybe String|])
    ] [''Show, ''Generic])

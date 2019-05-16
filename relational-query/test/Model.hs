{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module Model where

import GHC.Generics (Generic)
import Data.Int (Int32, Int64)

import Database.Relational (defaultConfig)
import Database.Relational.TH (defineTable, makeRelationalRecordDefault, defineScalarDegree)


$(defineTable defaultConfig "TEST" "set_a"
  [ ("int_a0" ,    [t| Int32  |])
  , ("str_a1" ,    [t| String |])
  , ("str_a2" ,    [t| String |]) ]
  [''Generic] [0] $ Just 0)


$(defineTable defaultConfig "TEST" "set_b"
  [ ("int_b0" ,    [t| Int32  |])
  , ("may_str_b1" ,    [t| Maybe String |])
  , ("str_b2" ,    [t| String |]) ]
  [''Generic] [0] $ Just 0)


$(defineTable defaultConfig "TEST" "set_c"
  [ ("int_c0" ,    [t| Int32  |])
  , ("str_c1" ,    [t| String |])
  , ("int_c2" ,    [t| Int64  |])
  , ("may_str_c3" ,    [t| Maybe String |]) ]
  [''Generic] [0] $ Just 0)


-- column name conflict with Confict.conflictB
$(defineTable defaultConfig "TEST" "conflict_a"
 [ ("foo" ,    [t| String |])
 , ("bar" ,    [t| Int32 |])
 , ("baz" ,    [t| Int32 |]) ]
 [''Generic] [0] $ Just 0)

$(defineTable defaultConfig "TEST" "set_i"
  [ ("int_i0" ,    [t| Int32 |])  ]
  [''Generic] [0] $ Just 0)

data ABC =
  ABC
  { xJustA :: SetA
  , xJustB :: SetB
  , xJustC :: SetC
  } deriving Generic

$(makeRelationalRecordDefault ''ABC)

data Abc =
  Abc
  { yJustA :: SetA
  , yMayB  :: Maybe SetB
  , yMayC  :: Maybe SetC
  } deriving Generic

$(makeRelationalRecordDefault ''Abc)

$(defineScalarDegree [t| Int32 |])

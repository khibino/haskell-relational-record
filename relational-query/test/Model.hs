{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Model where

import Data.Int (Int32, Int64)

import Database.Relational.Query (defaultConfig)
import Database.Relational.Query.TH (defineTableDefault, makeRelationalRecordDefault)


$(defineTableDefault defaultConfig "TEST" "set_a"
  [ ("int_a0" ,    [t| Int32  |])
  , ("str_a1" ,    [t| String |])
  , ("str_a2" ,    [t| String |]) ]
  [] [0] $ Just 0)


$(defineTableDefault defaultConfig "TEST" "set_b"
  [ ("int_b0" ,    [t| Int32  |])
  , ("may_str_b1" ,    [t| Maybe String |])
  , ("str_b2" ,    [t| String |]) ]
  [] [0] $ Just 0)


$(defineTableDefault defaultConfig "TEST" "set_c"
  [ ("int_c0" ,    [t| Int32  |])
  , ("str_c1" ,    [t| String |])
  , ("int_c2" ,    [t| Int64  |])
  , ("may_str_c3" ,    [t| Maybe String |]) ]
  [] [0] $ Just 0)


data ABC =
  ABC
  { xJustA :: SetA
  , xJustB :: SetB
  , xJustC :: SetC
  }

$(makeRelationalRecordDefault ''ABC)

data Abc =
  Abc
  { yJustA :: SetA
  , yMayB  :: Maybe SetB
  , yMayC  :: Maybe SetC
  }

$(makeRelationalRecordDefault ''Abc)

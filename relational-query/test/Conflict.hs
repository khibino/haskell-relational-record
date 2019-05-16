{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module Conflict where

import GHC.Generics (Generic)
import Data.Int (Int32)

import Database.Relational (defaultConfig)
import Database.Relational.TH (defineTable)


-- column name conflict with Model.conflictA
$(defineTable defaultConfig "TEST" "conflict_b"
  [ ("foo" ,    [t| Int32 |])
  , ("bar" ,    [t| String |])
  , ("baz" ,    [t| Int32 |]) ]
  [''Generic] [0] $ Just 0)

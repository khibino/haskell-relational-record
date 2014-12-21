{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Person where

import Data.Int
import Database.Relational.Query
import Database.Relational.Query.TH

$(defineTableDefault defaultConfig
  "PUBLIC" "person"
  [ ("name"   , [t| String |])
  , ("age"    , [t| Int32  |])
  , ("address", [t| String |])
  ]
  [] [0] (Just 0))

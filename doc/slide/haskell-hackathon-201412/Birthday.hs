{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Birthday where

import Data.Time
import Database.Relational.Query
import Database.Relational.Query.TH
import Database.HDBC.Query.TH ()

$(defineTable defaultConfig
  "PUBLIC" "birthday"
  [ ("name"   , [t| String |])
  , ("day"    , [t| Day    |])
  ]
  [] [0] (Just 0))

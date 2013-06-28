{-# LANGUAGE TemplateHaskell #-}

module Membership where

import Data.Int
import Database.Relational.Query.TH
import Database.Record.TH (derivingShow)

$(defineTableDefault'
  "SAMPLE1"
  "membership"
  [("user_id", [t|Int32|])
  ,("group_id", [t|Int32|])]
  [derivingShow])

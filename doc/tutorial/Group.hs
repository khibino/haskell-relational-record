{-# LANGUAGE TemplateHaskell #-}

module Group where

import Data.Int
import Database.Relational.Query.TH
import Database.Record.TH (derivingShow)

$(defineTableDefault'
  "SAMPLE1"
  "group"
  [("id", [t|Int32|])
  ,("name", [t|Maybe String|])]
  [derivingShow])

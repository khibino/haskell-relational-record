{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

import Test.QuickCheck.Simple (defaultMain, eqTest)

import Database.Record (toRecord)

import Model (User (..), Group (..), Membership (..))


main :: IO ()
main =
  defaultMain
  [ eqTest
    "nestedEq"
    (Membership { user  = User { uid = 1, uname = "Kei Hibino", note = "HRR developer" }
                , group = Just $ Group { gid = 1, gname = "Haskellers" }
                } )
    (toRecord ["1", "Kei Hibino", "HRR developer", "1", "Haskellers"]) ]

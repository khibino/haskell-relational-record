{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module HrrDatatypeTest where

import GHC.Generics (Generic)
import Language.Haskell.TH (runIO)
import Database.HDBC.Query.TH (defineTableFromDB)
import Database.HDBC.Schema.Oracle (driverOracle)

import DataSource (connect, getOwner)

$(do owner <- runIO getOwner
     defineTableFromDB connect driverOracle owner "hrr_datatype_test" [''Show, ''Generic])

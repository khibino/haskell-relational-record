{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module HrrDatatypeTest where

import Database.HDBC.Query.TH (defineTableFromDB)
import Database.HDBC.Schema.Oracle (driverOracle)
import Database.Record.TH (derivingShow)

import DataSource (connect, owner)

defineTableFromDB connect driverOracle owner "hrr_datatype_test" [derivingShow]

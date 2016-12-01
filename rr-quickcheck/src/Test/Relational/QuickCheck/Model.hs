{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Test.Relational.QuickCheck.Model (
  A (..), a0', a1', a2',
  B (..), b0', b1', b2',
  ) where

import Data.Int (Int64)
import Database.HDBC.Query.TH (makeRecordPersistableDefault)


data A =
  A
  { a0 :: Int64
  , a1 :: Int64
  , a2 :: Int64
  } deriving (Eq, Show)

data B =
  B
  { b0 :: Int64
  , b1 :: Int64
  , b2 :: Int64
  } deriving (Eq, Show)


$(makeRecordPersistableDefault ''A)
$(makeRecordPersistableDefault ''B)

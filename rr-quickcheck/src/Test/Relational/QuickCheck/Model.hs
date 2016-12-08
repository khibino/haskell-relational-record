{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Test.Relational.QuickCheck.Model (
  A (..), a0', a1', a2', relA,
  B (..), b0', b1', b2', relB,
  ) where

import Data.Int (Int64)
import qualified Database.Relational.Query.Table as Table
import Database.Relational.Query (Relation, table, TableDerivable (derivedTable))
import Database.HDBC.Query.TH (makeRecordPersistableDefault)


data A =
  A
  { a0 :: Int64
  , a1 :: Int64
  , a2 :: Int64
  } deriving (Eq, Ord, Show)

data B =
  B
  { b0 :: Int64
  , b1 :: Int64
  , b2 :: Int64
  } deriving (Eq, Ord, Show)


$(makeRecordPersistableDefault ''A)
$(makeRecordPersistableDefault ''B)

instance TableDerivable A where
  derivedTable = Table.table "ARBITRARY0.A" ["a0", "a1", "a2"]

relA :: Relation () A
relA = table derivedTable

instance TableDerivable B where
  derivedTable = Table.table "ARBITRARY0.B" ["b0", "b1", "b2"]

relB :: Relation () B
relB = table derivedTable

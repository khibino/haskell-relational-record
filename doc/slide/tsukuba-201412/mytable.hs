{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

import Data.Int

import Database.Relational.Query
import Database.Relational.Query.TH


$(defineTable defaultConfig
  "PUBLIC" "my_table"
  [ ("person", [t| String |])
  , ("family", [t| String |])
  , ("age"   , [t| Int32 |])
  ]
  [] [0] (Just 0))


agesOfFamilies :: Relation () (String, Maybe Int32)
agesOfFamilies =  aggregateRelation $ do
  my <- query myTable
  gFam <- groupBy $ my ! family'     -- Specify grouping key
  return $ gFam >< sum' (my ! age')  -- Aggregated results

agesOfFamiliesO :: Relation () (String, Maybe Int32)
agesOfFamiliesO =  aggregateRelation $ do
  my <- query myTable
  gFam <- groupBy $ my ! family'
  let s = sum' (my ! age')
  orderBy s Desc    -- Only aggregated value is allowd to pass
  orderBy gFam Asc
  return $ gFam >< s

ageRankOfFamilies :: Relation () ((Int64, String), Int32)
ageRankOfFamilies =  relation $ do
  my <- query myTable
  return $
    rank `over` do
      partitionBy $ my ! family'  -- Monad to build window
      orderBy (my ! age') Desc
    ><
    my ! family'
    ><
    my ! age'

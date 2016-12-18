module Test.Relational.QuickCheck.Tests (
  qPred1, qJoin1,
  ) where

import Test.QuickCheck (Property, ioProperty)
import Control.Monad (unless)
import Data.List (sort)
import Database.HDBC (IConnection, rollback)
import Database.HDBC.Session (withConnectionIO')
import Database.Relational.Query
import Database.HDBC.Record (runQuery')

import Test.Relational.QuickCheck.Transaction (initializeTable)
import Test.Relational.QuickCheck.Model
import Test.Relational.QuickCheck.Arbitrary
  (Selector (..), D(..), Pred (..), predSQL, predHask, Ranged (..), )


qPred1 :: IConnection conn
       => IO conn
       -> D (Pred A)
       -> Ranged A
       -> Property
qPred1 connect pa0 as0 = ioProperty . withConnectionIO' connect $ \conn -> do
  let pa = unD pa0
      select = relationalQuery . relation $ do
        x  <-  query relA
        wheres $ predSQL x pa
        orderBy x Asc
        return x
      as = runRanged as0
  print select

  initializeTable conn as
  qresult  <-  runQuery' conn select ()
  let expect =
        sort
        [ a
        | a  <-  as
        , predHask a pa
        ]
  print qresult
  print expect

  let judge = qresult == expect
  unless judge . putStr $ unlines [show qresult, "  =/=", show expect]
  rollback conn
  return judge

qJoin1 :: IConnection conn
       => IO conn
       -> Selector A
       -> Selector B
       -> Ranged A
       -> Ranged B
       -> Property
qJoin1 connect pa pb as0 bs0 = ioProperty . withConnectionIO' connect $ \conn -> do
  let select = relationalQuery . relation $ do
        x  <-  query relA
        y  <-  query relB
        on $ x ! sql pa .=. y ! sql pb
        orderBy x Asc
        orderBy y Asc
        return $ (,) |$| x |*| y
      as = runRanged as0
      bs = runRanged bs0
  initializeTable conn as
  initializeTable conn bs
  qresult  <-  runQuery' conn select ()
  let expect =
        sort
        [ (a, b)
        | a <- as
        , b <- bs
        , let x = int pa a
              y = int pb b
        , x == y
        ]
  let judge = qresult == expect
  unless judge . putStr $ unlines [show qresult, "  =/=", show expect]
  rollback conn
  return judge

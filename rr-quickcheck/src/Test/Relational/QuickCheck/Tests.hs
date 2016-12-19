{-# LANGUAGE FlexibleContexts #-}

module Test.Relational.QuickCheck.Tests (
  qPred1, qJoin1,
  ) where

import Test.QuickCheck (Property, ioProperty)
import Control.Monad (unless)
import Data.List (sort)
import Database.HDBC (IConnection, rollback, SqlValue)
import Database.HDBC.Session (withConnectionIO')
import Database.Record (FromSql, ToSql)
import Database.Relational.Query
import Database.HDBC.Record (runQuery', runInsert)

import Test.Relational.QuickCheck.Model
import Test.Relational.QuickCheck.Arbitrary
  (Selector (..), D(..), Pred (..), predSQL, predHask, Ranged (..), )


initializeTable :: (IConnection conn, TableDerivable a, ToSql SqlValue a)
                => conn
                -> [a]
                -> IO ()
initializeTable conn xs = mapM_ (runInsert conn $ derivedInsert id') xs

propQueryList :: (Eq a, Show a, FromSql SqlValue a, IConnection conn)
              => IO conn
              -> (conn -> IO ())
              -> Query () a
              -> [a]
              -> Property
propQueryList connect initialize select expect =
  ioProperty . withConnectionIO' connect $ \conn -> do
    initialize conn
    qresult  <-  runQuery' conn select ()
    let judge = qresult == expect
    unless judge . putStr $ unlines [show qresult, "  =/=", show expect]
    rollback conn
    return judge

qPred1 :: IConnection conn
       => IO conn
       -> D (Pred A)
       -> Ranged A
       -> Property
qPred1 connect pa0 as0 =
    propQueryList connect (`initializeTable` as) select expect
  where
    pa = unD pa0
    as = runRanged as0
    select = relationalQuery . relation $ do
      x  <-  query relA
      wheres $ predSQL x pa
      orderBy x Asc
      return x
    expect =
      sort
      [ a
      | a  <-  as
      , predHask a pa
      ]

qJoin1 :: IConnection conn
       => IO conn
       -> Selector A
       -> Selector B
       -> Ranged A
       -> Ranged B
       -> Property
qJoin1 connect pa pb as0 bs0 =
    propQueryList connect initialize select expect
  where
    as = runRanged as0
    bs = runRanged bs0
    initialize conn = do
      initializeTable conn as
      initializeTable conn bs
    select = relationalQuery . relation $ do
      x  <-  query relA
      y  <-  query relB
      on $ x ! sql pa .=. y ! sql pb
      orderBy x Asc
      orderBy y Asc
      return $ (,) |$| x |*| y
    expect =
      sort
      [ (a, b)
      | a <- as
      , b <- bs
      , let x = int pa a
            y = int pb b
      , x == y
      ]

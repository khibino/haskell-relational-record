{-# LANGUAGE FlexibleContexts #-}

module Test.Relational.QuickCheck.Tests (
  tests,

  prop_pred0,
  prop_join0, prop_join1,
  ) where

import Test.QuickCheck (Property)
import Test.QuickCheck.CompatIO (ioProperty)
import Test.QuickCheck.Simple (Test, qcTest)
import Control.Monad (unless)
import Data.List (sort)
import Data.Functor.ProductIsomorphic ((|$|), (|*|))

import Database.HDBC (IConnection, rollback, SqlValue)
import Database.HDBC.Session (withConnectionIO)
import Database.Record (FromSql, ToSql)
import Database.Relational
import Database.HDBC.Record (runQuery', runInsert)

import Test.Relational.QuickCheck.Model
import Test.Relational.QuickCheck.Arbitrary
  (Selector (..), D(..), Pred (..), predSQL, predHask, Cmp (..), cmpSQL, cmpHask, Ranged (..), )


initializeTable :: (IConnection conn, TableDerivable a, ToSql SqlValue a)
                => conn
                -> [a]
                -> IO ()
initializeTable conn xs = mapM_ (runInsert conn $ insert id') xs

propQueryResult :: (Eq a, Show a, FromSql SqlValue a, IConnection conn)
                => IO conn
                -> (conn -> IO ())
                -> Query () a
                -> [a]
                -> Property
propQueryResult connect initialize select expect =
  ioProperty . withConnectionIO connect $ \conn -> do
    initialize conn
    qresult  <-  runQuery' conn select ()
    let judge = qresult == expect
    unless judge . putStr $ unlines [show qresult, "  =/=", show expect]
    rollback conn
    return judge

prop_pred0 :: IConnection conn
           => IO conn
           -> D (Pred A)
           -> Ranged A
           -> Property
prop_pred0 connect pa0 as0 =
    propQueryResult connect (`initializeTable` as) select expect
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
      [ a | a  <-  as, predHask a pa ]

prop_join0 :: IConnection conn
           => IO conn
           -> Selector A
           -> Selector B
           -> Cmp
           -> Ranged A
           -> Ranged B
           -> Property
prop_join0 connect pa pb cmp as0 bs0 =
    propQueryResult connect initialize select expect
  where
    as = runRanged as0
    bs = runRanged bs0
    initialize conn = do
      initializeTable conn as
      initializeTable conn bs
    select = relationalQuery . relation $ do
      x  <-  query relA
      y  <-  query relB
      on $ cmpSQL cmp (x ! sql pa) (y ! sql pb)
      orderBy x Asc
      orderBy y Asc
      return $ (,) |$| x |*| y
    expect =
      sort
      [ (a, b)
      | a <- as, b <- bs
      , cmpHask cmp (int pa a) (int pb b)
      ]

prop_join1 :: IConnection conn
           => IO conn
           -> D (Pred A)
           -> D (Pred B)
           -> Ranged A
           -> Ranged B
           -> Property
prop_join1 connect pa0 pb0 as0 bs0 =
    propQueryResult connect initialize select expect
  where
    as = runRanged as0
    bs = runRanged bs0
    pa = unD pa0
    pb = unD pb0
    initialize conn = do
      initializeTable conn as
      initializeTable conn bs
    select = relationalQuery . relation $ do
      x  <-  query relA
      y  <-  query relB
      wheres $ predSQL x pa
      wheres $ predSQL y pb
      orderBy x Asc
      orderBy y Asc
      return $ (,) |$| x |*| y
    expect =
      sort
      [ (a, b)
      | a <- as, b <- bs
      , predHask a pa
      , predHask b pb
      ]

tests :: IConnection conn
      => IO conn
      -> [Test]
tests connect =
  [ qcTest "predicates 0"  $ prop_pred0 connect
  , qcTest "join 0"        $ prop_join0 connect
  , qcTest "join 1"        $ prop_join1 connect
  ]

{-# LANGUAGE FlexibleInstances #-}

module Test.Relational.QuickCheck.Arbitrary (
  Selector (..), qJoin1,
  ) where

import Test.QuickCheck (Arbitrary (..), Gen, elements, listOf, Property, ioProperty)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless)
import Data.Int (Int64)
import Data.List (sort)
import Database.HDBC (IConnection, rollback)
import Database.HDBC.Session (withConnectionIO')
import Database.Relational.Query
import Database.Relational.Query.Pi.Unsafe (unsafeExpandIndexes)
import Database.HDBC.Record (runQuery', runInsert)

import Test.Relational.QuickCheck.Model


data Selector r =
  Selector
  { int  :: r -> Int64
  , sql  :: Pi r Int64
  }

instance Show (Selector r) where
  show s = unwords ["Selector", show . unsafeExpandIndexes $ sql s]

genSelector :: [(r -> Int64, Pi r Int64)] -> Gen (Selector r)
genSelector = elements . map (uncurry Selector)

instance Arbitrary (Selector A) where
  arbitrary = genSelector [ (a0, a0'), (a1, a1'), (a2, a2') ]

instance Arbitrary (Selector B) where
  arbitrary = genSelector [ (b0, b0'), (b1, b1'), (b2, b2') ]

newtype Ranged a = Ranged { runRanged :: [a] }
  deriving (Eq, Show)

range10 :: (Num a, Enum a) => Gen a
range10 = elements [0 .. 10]

instance Arbitrary (Ranged A) where
  arbitrary =
    (Ranged <$>) . listOf $
    A
    <$> range10
    <*> range10
    <*> range10

instance Arbitrary (Ranged B) where
  arbitrary =
    (Ranged <$>) . listOf $
    B
    <$> range10
    <*> range10
    <*> range10

insertA :: Insert A
insertA = derivedInsert id'

insertB :: Insert B
insertB = derivedInsert id'

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
  mapM_ (runInsert conn insertA) as
  mapM_ (runInsert conn insertB) bs
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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

import Database.Record

import Database.Relational.Query
import Database.HDBC (IConnection, SqlValue)
import Data.Int (Int32)

import SetA (setA)
import qualified SetA
import SetB (setB)
import qualified SetB

import PgTestDataSource (connect)
import Database.HDBC.Record.Query (runQuery)
import Database.HDBC.Session (withConnectionIO, handleSqlError')

qa :: Relation () (Int32, String)
qa =  relation $ do
  a <- query setA
  return $ a ! SetA.seq' >< a ! SetA.name'

qb :: Relation () (Int32, String)
qb =  relation $ do
  b <- query setB
  return $ b ! SetB.seq' >< b ! SetB.name'

append :: (forall a . Relation () a -> Relation () a -> Relation () a)
       -> Relation () (Int32, String)
append op = relation $ do
  q <- query $ qa `op` qb
  asc $ q ! fst'
  return q

u :: Relation () (Int32, String)
u =  append union

e :: Relation () (Int32, String)
e =  append except

i :: Relation () (Int32, String)
i =  append intersect

i2 :: Relation () (Int32, String)
i2 =  relation $ do
  qu <- query $ qa `union`     qb
  qi <- query $ qa `intersect` qb

  wheres $ qu ! fst' .=. qi ! fst'

  return qu

runAndPrint :: (Show a, IConnection conn, FromSql SqlValue a, ToSql SqlValue p)
            => conn -> Relation p a -> p -> IO ()
runAndPrint conn rel param = do
  putStrLn $ "SQL: " ++ sqlFromRelation rel
  records  <- runQuery conn param (relationalQuery rel)
  mapM_ print records
  putStrLn ""

run :: IO ()
run =  handleSqlError' $ withConnectionIO connect
       (\conn -> do
           let run' :: (Show a, FromSql SqlValue a, ToSql SqlValue p)
                    => Relation p a -> p -> IO ()
               run' = runAndPrint conn
           run' qa ()
           run' qb ()
           run' u ()
           run' e ()
           run' i ()
           run' i2 ()
       )

main :: IO ()
main =  run

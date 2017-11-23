{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

import Data.Functor.ProductIsomorphic
import Database.Record

import Database.Relational
import Database.HDBC (IConnection, SqlValue)
import Data.Int (Int32)

import SetA (SetA (SetA), setA)
import qualified SetA
import SetB (setB)
import qualified SetB
import History (History (History), history)

import PgTestDataSource (connect)
import Database.HDBC.Record (runQuery)
import Database.HDBC.Session (withConnectionIO, handleSqlError')

qa' :: Relation () (Int32, String)
qa' =  relation $ do
  a <- query setA
  return $ a ! SetA.seq' >< a ! SetA.name'

qb' :: Relation () (Int32, String)
qb' =  relation $ do
  b <- query setB
  return $ b ! SetB.seq' >< b ! SetB.name'

append' :: (forall a . Relation () a -> Relation () a -> Relation () a)
       -> Relation () (Int32, String)
append' op = relation $ do
  q <- query $ qa' `op` qb'
  asc $ q ! fst'
  return q

u :: Relation () (Int32, String)
u =  append' union

e :: Relation () (Int32, String)
e =  append' except

i :: Relation () (Int32, String)
i =  append' intersect

qa :: Relation () SetA
qa =  setA

qb :: Relation () SetA
qb =  relation $ do
  b <- query setB
  return $ SetA |$| b ! SetB.seq' |*| b ! SetB.name'

append :: (forall a . Relation () a -> Relation () a -> Relation () a)
       -> Relation () SetA
append op = relation $ do
  q <- query $ qa `op` qb
  asc $ q ! SetA.seq'
  return q

uAll :: Relation () SetA
uAll =  append unionAll

i2 :: Relation () SetA
i2 =  relation $ do
  qu <- query $ qa `union`     qb
  qi <- query $ qa `intersect` qb

  wheres $ qu ! SetA.seq' .=. qi ! SetA.seq'

  return qu

oldest :: Relation () History
oldest =  relation .
  return $ History |$| value 0 |*| value (read "2012-12-01 00:00:00") |*| value "oldest"

runAndPrint :: (Show a, IConnection conn, FromSql SqlValue a, ToSql SqlValue p)
            => conn -> Relation p a -> p -> IO ()
runAndPrint conn rel param = do
  putStrLn $ "SQL: " ++ show rel
  records  <- runQuery conn (relationalQuery rel) param
  mapM_ print records
  putStrLn ""

run :: IO ()
run =  handleSqlError' $ withConnectionIO connect
       (\conn -> do
           let run' :: (Show a, FromSql SqlValue a, ToSql SqlValue p)
                    => Relation p a -> p -> IO ()
               run' = runAndPrint conn
           run' qa' ()
           run' qb' ()
           run' u ()
           run' uAll ()
           run' e ()
           run' i ()
           run' i2 ()
           run' (history `union` oldest) ()
       )

main :: IO ()
main =  run

{-# LANGUAGE FlexibleContexts #-}

import QueryExample

import Database.Record

import Database.Relational.Query
import Database.HDBC (IConnection, SqlValue)

import User (User)
import Group (Group)

import PgTestDataSource (connect)
import Database.HDBC.Record.Statement
  (ExecutedStatement, bindTo, execute)
import Database.HDBC.Record.Query
  (runQuery, prepare, fetchUnique, fetchUnique')
import Database.HDBC.Session (withConnectionIO, handleSqlError')


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
           run' userGroup0 ()
           run' userGroup0E ()
           run' haskellUser ()
           run' userGroup1 ()
           -- run' userGroup1E ()
           run' userGroup2 ()
           run' userGroup2E ()
           run' userGroupAggregate0 ()
           run' userGroupAggregate1 ()
           run' userGroupAggregate2 ()
           run' userGroup3 "Haskell"
           run' userGroupU ("Kei Hibino", "Haskell")
           run' userGroupStr ()
           run' windowRankByGroup ()
           run' specifiedUserAndGroup ()
           run' userGroupScalar ()
           run' groups ()
           run' userGroup2Fail ()
       )

runU :: Show a => (ExecutedStatement (User, Group) -> IO a) -> IO ()
runU f = handleSqlError' $ withConnectionIO connect
        (\conn -> do
            putStrLn $ "SQL: " ++ show userGroupU
            pq <- prepare conn (relationalQuery userGroupU)
            let bs = ("Kei Hibino", "Haskell") `bindTo` pq
            es <- execute bs
            r  <- f es
            print r
        )

runAll :: IO ()
runAll = do
  runU fetchUnique
  runU fetchUnique'
  run

main :: IO ()
main =  runAll

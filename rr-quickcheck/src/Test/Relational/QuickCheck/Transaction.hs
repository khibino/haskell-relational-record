{-# LANGUAGE FlexibleContexts #-}

module Test.Relational.QuickCheck.Transaction (
  initializeTable,
  )  where

import Database.HDBC (IConnection, SqlValue)
import Database.Record (ToSql)
import Database.Relational.Query
import Database.HDBC.Record (runInsert)


initializeTable :: (IConnection conn, TableDerivable a, ToSql SqlValue a)
                => conn
                -> [a]
                -> IO ()
initializeTable conn xs = mapM_ (runInsert conn $ derivedInsert id') xs

{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE FlexibleContexts #-}

import Database.Record

import Database.Relational.Query
import Database.HDBC (IConnection, SqlValue)
import Data.Int (Int32)

import qualified Account
import Account (Account(..), account)
import qualified Customer
import Customer (Customer, customer)
import qualified Individual
import Individual (Individual, individual)
import qualified ProductType
import ProductType (ProductType, productType)
import qualified Branch
import Branch (Branch, Branch)
import qualified Officer
import Officer (Officer, Officer)
import qualified Transaction
import Transaction (Transaction, transaction)
import qualified Business
import Business (Business, business)
import qualified Department
import Department (Department, department)
import qualified Product
import Product (Product, product)
import qualified Employee
import Employee (Employee, employee)

import DataSource (connect)
import Database.HDBC.Record.Query
  (ExecutedStatement,
   runQuery, prepare, bindTo, execute, fetchUnique, fetchUnique')
import Database.HDBC.Session (withConnectionIO, handleSqlError')

allAccount :: Relation () (Account)
allAccount =
  relation $
  [ a
  | a  <- query account
  ]

join1 :: Relation () (Employee, Department)
join1 =
  relation $
  [ e >< d
  | e <- query employee
  , d <- query department
  , () <- on $ e ! Employee.deptId' .=. just (d ! Department.deptId')
  ]

main :: IO ()
main = do
    print allAccount
    handleSqlError' $ withConnectionIO connect $ \conn -> do
        as <- runQuery conn () (fromRelation allAccount)
        mapM_ print as

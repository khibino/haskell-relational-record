{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

import Database.Record

import Database.Relational.Query
import Database.HDBC (IConnection, SqlValue, rollback)
import Database.HDBC.Query.TH (makeRecordPersistableDefault)
import Data.Int (Int64)
import Data.Time (Day, LocalTime)

import qualified Account
import Account (Account, account, tableOfAccount)
import qualified Customer
import Customer (Customer, customer)
--import qualified Individual
--import Individual (Individual, individual)
import qualified Product
import Product (product)
--import qualified ProductType
--import ProductType (ProductType, productType)
--import qualified Branch
--import Branch (Branch, Branch)
--import qualified Officer
--import Officer (Officer, Officer)
import qualified Transaction
import Transaction (transaction)
--import qualified Business
--import Business (Business, business)
import qualified Department
import Department (Department, department, tableOfDepartment)
--import qualified Product
--import Product (Product, product)
import qualified Employee
import Employee (Employee, employee, tableOfEmployee)

import DataSource (connect)
import Database.HDBC.Record (runDelete, runQuery, runUpdate)
import Database.HDBC.Session (withConnectionIO, handleSqlError')

import Prelude hiding (product)

allAccount :: Relation () Account
allAccount = relation $ query account

-- | sql/4.3.3a.sh
--
-- Handwritten SQL:
--
-- @
--   SELECT account_id, product_cd, cust_id, avail_balance
--   FROM LEARNINGSQL.account
--   WHERE product_cd IN ('CHK', 'SAV', 'CD', 'MM')
-- @
--
-- record version of Generated SQL:
--
-- @
--   SELECT ALL T0.account_id AS f0, T0.product_cd AS f1, T0.cust_id AS f2,
--   T0.open_date AS f3, T0.close_date AS f4, T0.last_activity_date AS f5,
--   T0.status AS f6, T0.open_branch_id AS f7, T0.open_emp_id AS f8,
--   T0.avail_balance AS f9, T0.pending_balance AS f10 FROM MAIN.account T0
--   WHERE (T0.product_cd IN ('CHK', 'SAV', 'CD', 'MM'))
-- @
--
account_4_3_3a :: Relation () Account
account_4_3_3a = relation $ do
  a  <- query account
  wheres $ a ! Account.productCd' `in'` values ["CHK", "SAV", "CD", "MM"]
  return a

-- |
-- tuple version of Generated SQL:
--
-- @
--   SELECT ALL T0.account_id AS f0, T0.product_cd AS f1, T0.cust_id AS f2,
--   T0.avail_balance AS f3 FROM MAIN.account T0 WHERE (T0.product_cd IN
--   ('CHK', 'SAV', 'CD', 'MM'))
-- @
--
account_4_3_3aT :: Relation () (((Int64, String), Int64), Maybe Double)
account_4_3_3aT = relation $ do
  a  <- query account
  wheres $ a ! Account.productCd' `in'` values ["CHK", "SAV", "CD", "MM"]
  return $ a ! Account.accountId' >< a ! Account.productCd' >< a ! Account.custId' >< a ! Account.availBalance'

-- |
-- Adhoc defined record version of Generated SQL:
--
-- @
--   SELECT ALL T0.account_id AS f0, T0.product_cd AS f1, T0.cust_id AS f2,
--   T0.avail_balance AS f3 FROM MAIN.account T0 WHERE (T0.product_cd IN
--   ('CHK', 'SAV', 'CD', 'MM'))
-- @
--
-- Above sql is the same to the tuple version.
--
account_4_3_3aR :: Relation () Account1
account_4_3_3aR = relation $ do
  a  <- query account
  wheres $ a ! Account.productCd' `in'` values ["CHK", "SAV", "CD", "MM"]
  return $ Account1 |$| a ! Account.accountId'
                    |*| a ! Account.productCd'
                    |*| a ! Account.custId'
                    |*| a ! Account.availBalance'

data Account1 = Account1
  { a1AccountId :: Int64
  , a1ProductCd :: String
  , a1CustId :: Int64
  , a1AvailBalance :: Maybe Double
  } deriving (Show)

$(makeRecordPersistableDefault ''Account1)

-- | sql/5.1.2a.sh
--
-- Handwritten SQL:
--
-- @
--   SELECT e.fname, e.lname, d.name
--   FROM LEARNINGSQL.employee e INNER JOIN LEARNINGSQL.department d
--   USING (dept_id)
-- @
--
-- Record version of Generated SQL:
--
-- @
--   SELECT ALL T0.emp_id AS f0, T0.fname AS f1, T0.lname AS f2,
--   T0.start_date AS f3, T0.end_date AS f4, T0.superior_emp_id AS f5,
--   T0.dept_id AS f6, T0.title AS f7, T0.assigned_branch_id AS f8,
--   T1.dept_id AS f9, T1.name AS f10 FROM MAIN.employee T0 INNER JOIN
--   MAIN.department T1 ON (T0.dept_id = T1.dept_id)
-- @
--
join_5_1_2a :: Relation () (Employee, Department)
join_5_1_2a = relation $ do
  e  <- query employee
  d  <- query department
  on $ e ! Employee.deptId' .=. just (d ! Department.deptId')
  return $ e >< d

-- |
-- Tuple version of Generated SQL:
--
-- @
--   SELECT ALL T0.fname AS f0, T0.lname AS f1, T1.name AS f2 FROM
--   MAIN.employee T0 INNER JOIN MAIN.department T1 ON (T0.dept_id
--   = T1.dept_id)
-- @
--
join_5_1_2aT :: Relation () ((String, String), String)
join_5_1_2aT = relation $ do
  e  <- query employee
  d  <- query department
  on $ e ! Employee.deptId' .=. just (d ! Department.deptId')
  return $ e ! Employee.fname' >< e ! Employee.lname' >< d ! Department.name'

-- | sql/5.3a.sh
--
-- Handwritten SQL:
--
-- @
--   SELECT e.fname, e.lname, e_mgr.fname mgr_fname, e_mgr.lname mgr_lname
--   FROM LEARNINGSQL.employee e INNER JOIN LEARNINGSQL.employee e_mgr
--   ON e.superior_emp_id = e_mgr.emp_id
-- @
--
-- Record version of Generated SQL:
--
-- @
--   SELECT ALL T0.emp_id AS f0, T0.fname AS f1, T0.lname AS f2,
--   T0.start_date AS f3, T0.end_date AS f4, T0.superior_emp_id AS f5,
--   T0.dept_id AS f6, T0.title AS f7, T0.assigned_branch_id AS f8,
--   T1.emp_id AS f9, T1.fname AS f10, T1.lname AS f11, T1.start_date AS
--   f12, T1.end_date AS f13, T1.superior_emp_id AS f14, T1.dept_id AS f15,
--   T1.title AS f16, T1.assigned_branch_id AS f17 FROM MAIN.employee T0
--   INNER JOIN MAIN.employee T1 ON (T0.superior_emp_id = T1.emp_id)
-- @
--
selfJoin_5_3a :: Relation () (Employee, Employee)
selfJoin_5_3a = relation $ do
  e  <- query employee
  m  <- query employee
  on $ e ! Employee.superiorEmpId' .=. just (m ! Employee.empId')
  return $ e >< m

-- |
-- Tuple version of Generated SQL:
--
-- @
--   SELECT ALL T0.fname AS f0, T0.lname AS f1, T1.fname AS f2, T1.lname AS
--   f3 FROM MAIN.employee T0 INNER JOIN MAIN.employee T1 ON
--   (T0.superior_emp_id = T1.emp_id)
-- @
--
selfJoin_5_3aT :: Relation () ((String, String), (String, String))
selfJoin_5_3aT = relation $ do
  e  <- query employee
  m  <- query employee
  on $ e ! Employee.superiorEmpId' .=. just (m ! Employee.empId')
  let emp = e ! Employee.fname' >< e ! Employee.lname'
  let mgr = m ! Employee.fname' >< m ! Employee.lname'
  return $ emp >< mgr

-- | sql/6.4.1a.sh
--
-- The standard SQL allows the syntax of UNION that has an order clause
-- at the last of query. Unfortunately, HRR dows not support. In addition,
-- HRR put a select statement having an order clause into parentheses.
-- Generated SQL has different meaning with the handwritten
-- SQL below. Such query cannot be expressed directly with EDSL of HRR.
--
-- Handwritten SQL:
--
-- @
--   SELECT emp_id, assigned_branch_id
--   FROM LEARNINGSQL.employee
--   WHERE title = 'Teller'
--   UNION
--   SELECT open_emp_id, open_branch_id
--   FROM LEARNINGSQL.account
--   WHERE product_cd = 'SAV'
--   ORDER BY open_emp_id
-- @
--
-- Generated SQL:
--
-- @
--   SELECT ALL T0.emp_id AS f0, T0.assigned_branch_id AS f1 FROM
--   MAIN.employee T0 WHERE (T0.title = 'Teller') UNION SELECT ALL
--   T1.open_emp_id AS f0, T1.open_branch_id AS f1 FROM MAIN.account T1
--   WHERE (T1.product_cd = 'SAV')
-- @
--
union_6_4_1a_Flat :: Relation () (Maybe Int64, Maybe Int64)
union_6_4_1a_Flat = relation (do
    e  <- query employee
    wheres $ e ! Employee.title' .=. just (value "Teller")
    return $ just (e ! Employee.empId') >< e ! Employee.assignedBranchId'
  ) `union` relation (do
    a  <- query account
    wheres $ a ! Account.productCd' .=. value "SAV"
    -- asc $ a ! Account.openEmpId'
    return $ a ! Account.openEmpId' >< a ! Account.openBranchId'
  )

-- |
-- If you want to sort whole row returned from UNION, place a order
-- clouse outside of the union relation.
--
-- Generated SQL:
--
-- @
--   SELECT ALL T2.f0 AS f0, T2.f1 AS f1 FROM (SELECT ALL T0.emp_id AS f0,
--   T0.assigned_branch_id AS f1 FROM MAIN.employee T0 WHERE (T0.title
--   = 'Teller') UNION SELECT ALL T1.open_emp_id AS f0, T1.open_branch_id
--   AS f1 FROM MAIN.account T1 WHERE (T1.product_cd = 'SAV')) T2 ORDER BY
--   T2.f0 ASC
-- @
--
union_6_4_1a_Nest :: Relation () (Maybe Int64, Maybe Int64)
union_6_4_1a_Nest = relation $ do
  ea <- query $ employee_6_4_1a `union` account_6_4_1a
  asc $ ea ! fst'
  return ea

employee_6_4_1a :: Relation () (Maybe Int64, Maybe Int64)
employee_6_4_1a = relation $ do
  e  <- query employee
  wheres $ e ! Employee.title' .=. just (value "Teller")
  return $ just (e ! Employee.empId') >< e ! Employee.assignedBranchId'

account_6_4_1a :: Relation () (Maybe Int64, Maybe Int64)
account_6_4_1a = relation $ do
  a  <- query account
  wheres $ a ! Account.productCd' .=. value "SAV"
  return $ a ! Account.openEmpId' >< a ! Account.openBranchId'

-- | sql/8.1a.sh
--
-- Handwritten SQL:
--
-- @
--   SELECT open_emp_id, COUNT(*) how_many
--   FROM LEARNINGSQL.account
--   GROUP BY open_emp_id
--   ORDER BY open_emp_id
-- @
--
-- Generated SQL:
--
-- @
--  SELECT ALL T0.open_emp_id AS f0, COUNT (T0.account_id) AS f1 FROM
--  MAIN.account T0 GROUP BY T0.open_emp_id ORDER BY T0.open_emp_id ASC
-- @
--
group_8_1a :: Relation () (Maybe Int64, Int64)
group_8_1a = aggregateRelation $ do
  a  <- query account
  g  <- groupBy $ a ! Account.openEmpId'
  asc $ g ! id'
  return $ g >< count (a ! Account.accountId')

-- | sql/4.3.3b.sh
--
-- Handwritten SQL:
--
-- @
--   SELECT account_id, product_cd, cust_id, avail_balance
--   FROM account
--   WHERE product_cd IN (SELECT product_cd FROM product
--   WHERE product_type_cd = 'ACCOUNT')
-- @
--
-- Record version of Generated SQL:
--
-- @
--   SELECT ALL T0.account_id AS f0, T0.product_cd AS f1, T0.cust_id AS f2,
--   T0.open_date AS f3, T0.close_date AS f4, T0.last_activity_date AS f5,
--   T0.status AS f6, T0.open_branch_id AS f7, T0.open_emp_id AS f8,
--   T0.avail_balance AS f9, T0.pending_balance AS f10 FROM MAIN.account T0
--   WHERE (T0.product_cd IN (SELECT ALL T1.product_cd AS f0 FROM
--   MAIN.product T1 WHERE (T1.product_type_cd = ?)))
-- @
--
account_4_3_3b :: Relation String Account
account_4_3_3b = relation' $ do
  a <- query account
  (phProductCd,p) <- queryList' product_4_3_3b
  wheres $ a ! Account.productCd' `in'` p
  return (phProductCd, a)

-- |
-- Tuple version of Generated SQL:
--
-- @
--   SELECT ALL T0.account_id AS f0, T0.product_cd AS f1, T0.cust_id AS f2,
--   T0.avail_balance AS f3 FROM MAIN.account T0 WHERE (T0.product_cd IN
--   (SELECT ALL T1.product_cd AS f0 FROM MAIN.product T1 WHERE
--   (T1.product_type_cd = ?)))
-- @
--
account_4_3_3bT :: Relation String (((Int64, String), Int64), Maybe Double)
account_4_3_3bT = relation' $ do
  a <- query account
  (phProductCd,p) <- queryList' product_4_3_3b
  wheres $ a ! Account.productCd' `in'` p
  let at = a ! Account.accountId' >< a ! Account.productCd' >< a ! Account.custId' >< a ! Account.availBalance'
  return (phProductCd, at)

-- |
-- Adhoc record version of Generated SQL:
--
-- @
--   SELECT ALL T0.account_id AS f0, T0.product_cd AS f1, T0.cust_id AS f2,
--   T0.avail_balance AS f3 FROM MAIN.account T0 WHERE (T0.product_cd IN
--   (SELECT ALL T1.product_cd AS f0 FROM MAIN.product T1 WHERE
--   (T1.product_type_cd = ?)))
-- @
--
account_4_3_3bR :: Relation String Account1
account_4_3_3bR = relation' $ do
  a <- query account
  (phProductCd,p) <- queryList' product_4_3_3b
  wheres $ a ! Account.productCd' `in'` p
  let ar = Account1 |$| a ! Account.accountId'
                    |*| a ! Account.productCd'
                    |*| a ! Account.custId'
                    |*| a ! Account.availBalance'
  return (phProductCd, ar)

product_4_3_3b :: Relation String String
product_4_3_3b = relation' $ do
  p <- query product
  (phProductCd,()) <- placeholder (\ph -> wheres $ p ! Product.productTypeCd' .=. ph)
  let productCd = p ! Product.productCd'
  return (phProductCd, productCd)

-- | sql/4.3.3c.sh
--
-- Handwritten SQL:
--
-- @
--   SELECT account_id, product_cd, cust_id, avail_balance
--   FROM LEARNINGSQL.account
--   WHERE product_cd NOT IN ('CHK', 'SAV', 'CD', 'MM')
-- @
--
-- Generated SQL:
--
-- @
--   SELECT ALL T0.account_id AS f0, T0.product_cd AS f1, T0.cust_id AS f2,
--   T0.open_date AS f3, T0.close_date AS f4, T0.last_activity_date AS f5,
--   T0.status AS f6, T0.open_branch_id AS f7, T0.open_emp_id AS f8,
--   T0.avail_balance AS f9, T0.pending_balance AS f10 FROM MAIN.account T0
--   WHERE (NOT (T0.product_cd IN ('CHK', 'SAV', 'CD', 'MM')))
-- @
--
account_4_3_3c :: Relation () Account
account_4_3_3c = relation $ do
  a  <- query account
  wheres $ not' (a ! Account.productCd' `in'` values ["CHK", "SAV", "CD", "MM"])
  return a

-- | sql/3.7
--
-- Handwritten SQL:
--
-- @
--   SELECT open_emp_id, product_cd
--   FROM account
--   ORDER BY open_emp_id, product_cd
-- @
--
-- Generated SQL:
--
-- @
--   SELECT ALL T0.open_emp_id AS f0, T0.product_cd AS f1 FROM MAIN.account
--   T0 ORDER BY T0.open_emp_id ASC, T0.product_cd ASC
-- @
--
account_3_7 :: Relation () (Maybe Int64, String)
account_3_7 = relation $ do
  a <- query account
  let proj = (,) |$| a ! Account.openEmpId'
                 |*| a ! Account.productCd'
  asc proj
  return proj

-- | sql/3.7.1
--
-- Handwritten SQL:
--
-- @
--   SELECT account_id, product_cd, open_date, avail_balance
--   FROM account
--   ORDER BY avail_balance DESC
-- @
--
-- Generated SQL:
--
-- @
--   SELECT ALL T0.account_id AS f0, T0.product_cd AS f1, T0.open_date AS
--   f2, T0.avail_balance AS f3 FROM MAIN.account T0 ORDER BY
--   T0.avail_balance DESC
-- @
--
account_3_7_1 :: Relation () Account2
account_3_7_1 = relation $ do
  a <- query account
  desc $ a ! Account.availBalance'
  return $ Account2 |$| a ! Account.accountId'
                    |*| a ! Account.productCd'
                    |*| a ! Account.openDate'
                    |*| a ! Account.availBalance'

data Account2 = Account2
  { a2AccountId :: Int64
  , a2ProductCd :: String
  , a2OpenDate :: Day
  , a2AvailBalance :: Maybe Double
  } deriving (Show)

$(makeRecordPersistableDefault ''Account2)

-- | sql/3.7.3
--
-- For backwards compatibility with the SQL92 version of standard, you can
-- use numbers instead of names to specify the columns that should be sorted.
-- With HRR you cannot use numbers for such purpose.
--
-- Handwritten SQL:
--
-- @
--   SELECT emp_id, title, start_date, fname, lname
--   FROM employee
--   ORDER BY 2,5
-- @
--
-- Generated SQL:
--
-- @
--   SELECT ALL T0.emp_id AS f0, T0.title AS f1, T0.start_date AS f2,
--   T0.fname AS f3, T0.lname AS f4 FROM MAIN.employee T0 ORDER BY T0.title
--   ASC, T0.lname ASC
-- @
--
employee_3_7_3 :: Relation () Employee1
employee_3_7_3 = relation $ do
  e <- query employee
  asc $ e ! Employee.title'
  asc $ e ! Employee.lname'
  return $ Employee1 |$| e ! Employee.empId'
                     |*| e ! Employee.title'
                     |*| e ! Employee.startDate'
                     |*| e ! Employee.fname'
                     |*| e ! Employee.lname'

data Employee1 = Employee1
  { e1EmpId :: Int64
  , e1Title :: Maybe String
  , e1StartDate :: Day
  , e1Fname :: String
  , e1Lname' :: String
  } deriving (Show)

$(makeRecordPersistableDefault ''Employee1)

-- | sql/4.1.2
--
-- HRR supports date literal of the SQL standard, such like Date '2003-01-01'.
-- However, SQLite has its own date literal without Date keyword,
-- like this: '2003-01-01'. So, you have to define a function to support
-- SQLite's date literal. Here we define 'unsafeSQLiteDayValue' function
-- for that.
--
-- Handwritten SQL:
--
-- @
--   SELECT *
--   FROM employee
--   WHERE end_date IS NULL AND (title = 'Teller' OR start_date < '2003-01-01')
-- @
--
-- Literal version of Generated SQL:
--
-- @
--   SELECT ALL T0.emp_id AS f0, T0.fname AS f1, T0.lname AS f2,
--   T0.start_date AS f3, T0.end_date AS f4, T0.superior_emp_id AS f5,
--   T0.dept_id AS f6, T0.title AS f7, T0.assigned_branch_id AS f8 FROM
--   MAIN.employee T0 WHERE ((T0.end_date IS NULL) AND ((T0.title
--   = 'Teller') OR (T0.start_date < '2003-01-01')))
-- @
--
employee_4_1_2 :: Relation () Employee
employee_4_1_2 = relation $ do
  e <- query employee
  wheres $ isNothing (e ! Employee.endDate')
  wheres $ e ! Employee.title' .=. just (value "Teller")
     `or'` e ! Employee.startDate' .<. unsafeSQLiteDayValue "2003-01-01"
  return e

unsafeSQLiteDayValue :: SqlProjectable p => String -> p Day
unsafeSQLiteDayValue = unsafeProjectSqlTerms . showConstantTermsSQL

-- |
-- Another way, use a placeholder instead of a date literal.
-- There is no need to define a helper function.
--
-- Placeholder version of Generated SQL:
--
-- @
--   SELECT ALL T0.emp_id AS f0, T0.fname AS f1, T0.lname AS f2,
--   T0.start_date AS f3, T0.end_date AS f4, T0.superior_emp_id AS f5,
--   T0.dept_id AS f6, T0.title AS f7, T0.assigned_branch_id AS f8 FROM
--   MAIN.employee T0 WHERE ((T0.end_date IS NULL) AND ((T0.title
--   = 'Teller') OR (T0.start_date < ?)))
-- @
--
employee_4_1_2P :: Relation Day Employee
employee_4_1_2P = relation' $ do
  e <- query employee
  wheres $ isNothing (e ! Employee.endDate')
  (phDay,()) <- placeholder (\ph ->
    wheres $ e ! Employee.title' .=. just (value "Teller")
       `or'` e ! Employee.startDate' .<. ph)
  return (phDay, e)

-- | sql/4.3.2
--
-- Handwritten SQL:
--
-- @
--   SELECT emp_id, fname, lname, start_date FROM employee
--   WHERE start_date
--   BETWEEN date('2001-01-01') AND date('2002-12-31')
-- @
--
-- Literal version of Generated SQL:
--
-- @
--   SELECT ALL T0.emp_id AS f0, T0.fname AS f1, T0.lname AS f2,
--   T0.start_date AS f3 FROM MAIN.employee T0 WHERE ((T0.start_date >=
--   '2001-01-01') AND (T0.start_date <= '2003-01-01'))
-- @
--
employee_4_3_2 :: Relation () Employee2
employee_4_3_2 = relation $ do
  e <- query employee
  wheres $ e ! Employee.startDate' .>=. unsafeSQLiteDayValue "2001-01-01"
  wheres $ e ! Employee.startDate' .<=. unsafeSQLiteDayValue "2003-01-01"
  return $ Employee2 |$| e ! Employee.empId'
                     |*| e ! Employee.fname'
                     |*| e ! Employee.lname'
                     |*| e ! Employee.startDate'

-- |
-- Placeholder version of Generated SQL:
--
-- Generated SQL:
--
-- @
--   SELECT ALL T0.emp_id AS f0, T0.fname AS f1, T0.lname AS f2,
--   T0.start_date AS f3 FROM MAIN.employee T0 WHERE ((T0.start_date >= ?)
--   AND (T0.start_date <= ?))
-- @
--
-- NOTE: Be careful on the order of the placeholders. You must give day
-- values in order that they appear on the generated SQL.
--
employee_4_3_2P :: Relation (Day,Day) Employee2
employee_4_3_2P = relation' $ do
  e <- query employee
  (phDay1,()) <- placeholder (\ph -> wheres $ e ! Employee.startDate' .>=. ph)
  (phDay2,()) <- placeholder (\ph -> wheres $ e ! Employee.startDate' .<=. ph)
  return (phDay1 >< phDay2,
           Employee2 |$| e ! Employee.empId'
                     |*| e ! Employee.fname'
                     |*| e ! Employee.lname'
                     |*| e ! Employee.startDate')

data Employee2 = Employee2
  { e2EmpId :: Int64
  , e2Fname :: String
  , e2Lname :: String
  , e2StartDate :: Day
  } deriving (Show)

$(makeRecordPersistableDefault ''Employee2)

-- |
-- 9.1 What is a subquery?
--
-- Handwritten SQL:
--
-- @
--   SELECT account_id, product_cd, cust_id, avail_balance
--   FROM account
--   WHERE account_id = (SELECT MAX(account_id)
--                       FROM account);
-- @
--
-- Generated SQL:
--
-- @
--   SELECT ALL T0.account_id AS f0, T0.product_cd AS f1, T0.cust_id AS f2,
--   T0.avail_balance AS f3 FROM MAIN.account T0 WHERE (T0.account_id
--   = (SELECT ALL MAX (T1.account_id) AS f0 FROM MAIN.account T1))
-- @
--
account_9_1 :: Relation () Account1
account_9_1 = relation $ do
  a  <- query account
  ma <- queryScalar $ aggregatedUnique account Account.accountId' max'
  wheres $ just (a ! Account.accountId') .=. flattenMaybe ma
  return $ Account1 |$| a ! Account.accountId'
                    |*| a ! Account.productCd'
                    |*| a ! Account.custId'
                    |*| a ! Account.availBalance'

-- |
-- 9.4 Correlated Subqueries
--
-- Handwritten SQL:
--
-- @
--   SELECT c.cust_id, c.cust_type_cd, c.city
--   FROM customer c
--   WHERE 2 = (SELECT COUNT(*)
--              FROM account a
--              WHERE a.cust_id = c.cust_id);
-- @
--
-- Generated SQL:
--
-- @
--   SELECT ALL T0.cust_id AS f0, T0.cust_type_cd AS f1, T0.city AS f2 FROM
--   MAIN.customer T0 WHERE (2 = (SELECT ALL COUNT (T2.f0) AS f0 FROM
--   (SELECT ALL T1.account_id AS f0 FROM MAIN.account T1 WHERE (T1.cust_id
--   = T0.cust_id)) T2))
-- @
--
customer_9_4 :: Relation () Customer1
customer_9_4 = relation $ do
  c  <- query customer
  ca <- queryScalar $ aggregatedUnique (relation $ do
    a <- query account
    wheres $ a ! Account.custId' .=. c ! Customer.custId'
    return (a ! Account.accountId')
    ) id' count
  wheres $ just (value 2) .=. ca
  return (customer1 c)

data Customer1 = Customer1
  { c1Custid :: Int64
  , c1CustTypeCd :: String
  , c1City :: Maybe String
  } deriving (Show)

customer1 :: (SqlProjectable (Projection c), ProjectableShowSql (Projection c))
          => Projection c Customer -> Projection c Customer1
customer1 c = Customer1 |$| c ! Customer.custId'
                        |*| c ! Customer.custTypeCd'
                        |*| c ! Customer.city'

$(makeRecordPersistableDefault ''Customer1)

-- |
-- (original) Deleting data
-- 
-- Handwritten SQL:
--
-- @
--   DELETE FROM account
--   WHERE account_id = 2;
-- @
--
-- Generated SQL:
--
-- @
--   DELETE FROM MAIN.account WHERE (account_id = 2)
-- @
--
deleteAccount_o1 :: Delete ()
deleteAccount_o1 = typedDelete tableOfAccount . restriction $ \proj -> do
  wheres $ proj ! Account.accountId' .=. value 2

-- |
-- (original) Data modification using equality conditions
--
-- Handwritten SQL:
--
-- @
--   DELETE FROM account
--   WHERE account_id >= 10 AND account_id <= 20;
-- @
--
-- Generated SQL:
--
-- @
--   DELETE FROM MAIN.account WHERE ((account_id >= 10) AND (account_id <=
--   20))
-- @
--
deleteAccount_o2 :: Delete ()
deleteAccount_o2 = typedDelete tableOfAccount . restriction $ \proj -> do
  wheres $ proj ! Account.accountId' .>=. value 10
  wheres $ proj ! Account.accountId' .<=. value 20


-- |
-- 9.4.2 Data manipulation using correlated subqueries
-- 
-- Handwritten SQL:
--
-- @
--   DELETE FROM department d
--   WHERE NOT EXISTS (SELECT 1
--   FROM employee e
--   WHERE e.dept_id = d.dept_id);
-- @
-- 
-- Generated SQL:
--
-- @
--   DELETE FROM MAIN.department WHERE (NOT (EXISTS (SELECT ALL 1 AS f0
--   FROM MAIN.employee T0 WHERE (T0.dept_id = dept_id))))
-- @
--
deleteEmployee_9_4_2 :: Delete ()
deleteEmployee_9_4_2 = typedDelete tableOfDepartment . restriction $ \proj -> do
  el <- queryList $ relation $ do
    e <- query employee
    wheres $ e ! Employee.deptId' .=. just (proj ! Department.deptId')
    return (value (1 :: Int64))
  wheres $ not' . exists $ el

-- |
-- (original) Updating data
--
-- Handwritten SQL:
--
-- @
--   UPDATE employee
--   SET lname = 'Bush',
--        dept_id = 3
--   WHERE emp_id = 10;
-- @
--
-- Generated SQL:
--
-- @
--   UPDATE MAIN.employee SET lname = 'Bush', dept_id = 3 WHERE (emp_id
--   = 10)
-- @
--
updateEmployee_o3 :: Update ()
updateEmployee_o3 = typedUpdate tableOfEmployee . updateTarget $ \proj -> do
  Employee.lname' <-# value "Bush"
  Employee.deptId' <-# just (value 3)
  wheres $ proj ! Employee.empId' .=. value 10

-- |
-- 9.4.2 Data Manipulation Using Correlated Subqueries
--
-- Handwritten SQL:
--
-- @
--   UPDATE account
--   SET last_activity_date =
--      (SELECT MAX(t.txn_date)
--       FROM transaction0 t
--       WHERE t.account_id = account.account_id)
--   WHERE EXISTS (SELECT 1
--                 FROM transaction0 t
--                 WHERE t.account_id = account.account_id);
-- @
--
-- Generated SQL:
--
-- @
--   UPDATE MAIN.account SET last_activity_date = date((SELECT ALL MAX
--   (T1.f0) AS f0 FROM (SELECT ALL T0.txn_date AS f0 FROM
--   MAIN.transaction0 T0 WHERE (T0.account_id = account_id)) T1)) WHERE
--   (EXISTS (SELECT ALL 1 AS f0 FROM MAIN.transaction0 T2 WHERE
--   (T2.account_id = account_id)))
-- @
--
updateAccount_9_4_2 :: Update ()
updateAccount_9_4_2 = typedUpdate tableOfAccount . updateTarget $ \proj -> do
  ts <- queryScalar $ aggregatedUnique (relation $ do
    t <- query transaction
    wheres $ t ! Transaction.accountId' .=. proj ! Account.accountId'
    return (t ! Transaction.txnDate')
    ) id' max'
  tl <- queryList $ relation $ do
    t <- query transaction
    wheres $ t ! Transaction.accountId' .=. proj ! Account.accountId'
    return (value (1 :: Int64))
  Account.lastActivityDate' <-# (toDay $ flattenMaybe ts)
  wheres $ exists $ tl

toDay :: (SqlProjectable p, ProjectableShowSql p) => p (Maybe LocalTime) -> p (Maybe Day)
toDay dt = unsafeProjectSql $ "date(" ++ unsafeShowSql dt ++ ")"

--
-- run and print sql
--

run :: (Show a, IConnection conn, FromSql SqlValue a, ToSql SqlValue p)
    => conn -> p -> Relation p a -> IO ()
run conn param rel = do
  putStrLn $ "SQL: " ++ show rel
  records <- runQuery conn (relationalQuery rel) param
  mapM_ print records
  putStrLn ""
  rollback conn

runU :: (IConnection conn, ToSql SqlValue p)
     => conn -> p -> Update p -> IO ()
runU conn param upd = do
  putStrLn $ "SQL: " ++ show upd
  num <- runUpdate conn upd param
  print num
  putStrLn ""
  rollback conn

runD :: (IConnection conn, ToSql SqlValue p)
     => conn -> p -> Delete p -> IO ()
runD conn param dlt = do
  putStrLn $ "SQL: " ++ show dlt
  num <- runDelete conn dlt param
  print num
  putStrLn ""
  rollback conn

main :: IO ()
main = handleSqlError' $ withConnectionIO connect $ \conn -> do
  run conn () allAccount
  run conn () account_4_3_3a
  run conn () account_4_3_3aT
  run conn () account_4_3_3aR
  run conn () join_5_1_2a
  run conn () join_5_1_2aT
  run conn () selfJoin_5_3a
  run conn () selfJoin_5_3aT
  run conn () union_6_4_1a_Nest
  run conn () union_6_4_1a_Flat
  run conn () group_8_1a
  run conn "ACCOUNT" account_4_3_3b
  run conn "ACCOUNT" account_4_3_3bT
  run conn "ACCOUNT" account_4_3_3bR
  run conn () account_3_7
  run conn () account_3_7_1
  run conn () employee_3_7_3
  run conn () employee_4_1_2
  run conn (read "2003-01-01") employee_4_1_2P
  run conn () employee_4_3_2
  run conn (read "2001-01-01", read "2003-01-01") employee_4_3_2P
  run conn () account_9_1
  run conn () customer_9_4
  runD conn () deleteAccount_o1
  runD conn () deleteAccount_o2
  runD conn () deleteEmployee_9_4_2
  runU conn () updateEmployee_o3
  runU conn () updateAccount_9_4_2


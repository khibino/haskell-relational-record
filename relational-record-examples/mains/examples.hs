{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

import Database.Relational.CustomSQLite3
import Database.Relational.OverloadedInstances ()
import Database.Relational.TupleInstances ()

import GHC.Generics (Generic)
import Prelude hiding (product)
import Data.Int (Int64)
import Data.Time (Day, LocalTime)
import Data.Functor.ProductIsomorphic ((|$|), (|*|))

import qualified Account
import Account (Account, account)
import qualified Branch
import Branch (Branch, branch)
import Business (business)
import Customer (Customer, customer)
import Department (Department, department)
import Individual (individual)
--import qualified Officer
--import Officer (Officer, Officer)
import Product (product)
--import qualified ProductType
--import ProductType (ProductType, productType)
import qualified Transaction
import Employee (Employee, employee)

allAccount :: Relation () Account
allAccount = relation $ query account

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
account_3_7 :: Relation () (Maybe Int, String)
account_3_7 = relation $ do
  a <- query account
  let proj = (,) |$| #openEmpId a
                 |*| #productCd a
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
  desc $ #availBalance a
  return $ Account2 |$| #accountId a
                    |*| #productCd a
                    |*| #openDate a
                    |*| #availBalance a

data Account2 = Account2
  { a2AccountId :: Int
  , a2ProductCd :: String
  , a2OpenDate :: Day
  , a2AvailBalance :: Maybe Double
  } deriving (Show, Generic)

$(makeRelationalRecord ''Account2)

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
  asc $ #title e
  asc $ #lname e
  return $ Employee1 |$| #empId e
                     |*| #title e
                     |*| #startDate e
                     |*| #fname e
                     |*| #lname e

data Employee1 = Employee1
  { e1EmpId :: Int
  , e1Title :: Maybe String
  , e1StartDate :: Day
  , e1Fname :: String
  , e1Lname' :: String
  } deriving (Show, Generic)

$(makeRelationalRecord ''Employee1)

-- | sql/4.1.2
--
-- HRR supports date literal of the SQL standard, such like DATE '2003-01-01'.
-- However, SQLite has its own date literal without DATE keyword,
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
  wheres $ isNothing (#endDate e)
  wheres $ #title e .=. just (toFlat $ value "Teller")
     `or'` #startDate e .<. unsafeSQLiteDayValue "2003-01-01"
  return e

unsafeSQLiteDayValue :: SqlContext c => String -> Record c Day
unsafeSQLiteDayValue = unsafeProjectSqlTerms . showLiteral

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
employee_4_1_2P = relation' $ \ph -> do
  e <- query employee
  wheres $ isNothing (#endDate e)
  wheres $ #title e .=. just (toFlat $ value "Teller")
     `or'` #startDate e .<. toFlat ph
  return e

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
  wheres $ #startDate e .>=. unsafeSQLiteDayValue "2001-01-01"
  wheres $ #startDate e .<=. unsafeSQLiteDayValue "2003-01-01"
  return $ Employee2 |$| #empId e
                     |*| #fname e
                     |*| #lname e
                     |*| #startDate e

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
employee_4_3_2P = relation' $ \ph -> do
  e <- query employee
  let date = #startDate e
  wheres $ date .>=. (toFlat $ (! #fst) ph)
  wheres $ date .<=. (toFlat $ (! #snd) ph)
  return $ Employee2 |$| #empId e
                     |*| #fname e
                     |*| #lname e
                     |*| date

data Employee2 = Employee2
  { e2EmpId :: Int
  , e2Fname :: String
  , e2Lname :: String
  , e2StartDate :: Day
  } deriving (Show, Generic)

$(makeRelationalRecord ''Employee2)

-- | sql/4.3.3a
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
  wheres $ #productCd a `in'` values ["CHK", "SAV", "CD", "MM"]
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
account_4_3_3aT :: Relation () (Int, String, Int, Maybe Double)
account_4_3_3aT = relation $ do
  a  <- query account
  wheres $ #productCd a `in'` values ["CHK", "SAV", "CD", "MM"]
  return $ (,,,) |$| #accountId a |*| #productCd a |*| #custId a |*| #availBalance a

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
  wheres $ #productCd a `in'` values ["CHK", "SAV", "CD", "MM"]
  return $ Account1 |$| #accountId a
                    |*| #productCd a
                    |*| #custId a
                    |*| #availBalance a

data Account1 = Account1
  { a1AccountId :: Int
  , a1ProductCd :: String
  , a1CustId :: Int
  , a1AvailBalance :: Maybe Double
  } deriving (Show, Generic)

$(makeRelationalRecord ''Account1)

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
  ma <- queryScalar $ aggregatedUnique account #accountId max'
  wheres $ just (#accountId a) .=. flattenMaybe ma
  return $ Account1 |$| #accountId a
                    |*| #productCd a
                    |*| #custId a
                    |*| #availBalance a

-- | sql/4.3.3b
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
account_4_3_3b = relation' $ \ph -> do
  a <- query account
  p <- queryList' ph product_4_3_3b
  wheres $ #productCd a `in'` p
  return a

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
account_4_3_3bT :: Relation String (Int, String, Int, Maybe Double)
account_4_3_3bT = relation' $ \ph -> do
  a <- query account
  p <- queryList' ph product_4_3_3b
  wheres $ #productCd a `in'` p
  let at = (,,,) |$| #accountId a |*| #productCd a |*| #custId a |*| #availBalance a
  return at

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
account_4_3_3bR = relation' $ \ph -> do
  a <- query account
  p <- queryList' ph product_4_3_3b
  wheres $ #productCd a `in'` p
  let ar = Account1 |$| #accountId a
                    |*| #productCd a
                    |*| #custId a
                    |*| #availBalance a
  return ar

product_4_3_3b :: Relation String String
product_4_3_3b = relation' $ \ph -> do
  p <- query product
  wheres $ #productTypeCd p .=. toFlat ph
  return $ #productCd p

-- | sql/4.3.3c
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
  wheres $ not' (#productCd a `in'` values ["CHK", "SAV", "CD", "MM"])
  return a

-- | sql/5.1.2a
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
  on $ #deptId e .=. just (#deptId d)
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
join_5_1_2aT :: Relation () (String, String, String)
join_5_1_2aT = relation $ do
  e  <- query employee
  d  <- query department
  on $ #deptId e .=. just (#deptId d)
  return $ (,,) |$| #fname e |*| #lname e |*| #name d

-- |
-- Left Outer Join
--
-- Handwritten SQL:
--
-- @
--  SELECT a.account_id, a.cust_id, i.fname, i.lname
--    FROM account a LEFT OUTER JOIN individual i
--      ON a.cust_id = i.cust_id
-- @
--
-- Generated SQL:
-- @
--   SELECT ALL T0.account_id AS f0, T0.cust_id AS f1, T1.fname AS f2,
--   T1.lname AS f3 FROM MAIN.account T0 LEFT JOIN MAIN.individual T1 ON
--   (T0.cust_id = T1.cust_id)
-- @
--
account_LeftOuterJoin :: Relation () Account4
account_LeftOuterJoin = relation $ do
  a <- query account
  i <- queryMaybe individual
  on $ just (#custId a) .=. (? #custId) i
  return $ Account4 |$| #accountId a
                    |*| #custId a
                    |*| (? #fname) i
                    |*| (? #lname) i

data Account4 = Account4
  { a4AccountId :: Int
  , a4CustId :: Int
  , a4Fname :: Maybe String
  , a4Lname :: Maybe String
  } deriving (Show, Generic)

$(makeRelationalRecord ''Account4)

-- |
-- Right Outer Join
--
-- Handwritten SQL:
--
-- @
-- SELECT c.cust_id, b.name
--   FROM customer c RIGHT OUTER JOIN business b
--       ON c.cust_id = b.cust_id
-- @
--
-- Generated SQL:
--
-- @
--   SELECT ALL T0.cust_id AS f0, T1.name AS f1 FROM MAIN.customer T0 RIGHT
--   JOIN MAIN.business T1 ON (T0.cust_id = T1.cust_id)
-- @
--
-- Note: A function using right-out-join can be defined, but unfortunately
-- SQLite3 does not support it.
--
business_RightOuterJoin :: Relation () (Maybe Int, String)
business_RightOuterJoin = relation $ do
  c <- queryMaybe customer
  b <- query business
  on $ (? #custId) c .=. just (#custId b)
  return ((? #custId) c >< #name b)

-- | sql/5.1.3
--
-- Handwritten SQL:
--
-- @
--   SELECT a.account_id, a.cust_id, a.open_date, a.product_cd
--   FROM account a INNER JOIN employee e ON a.open_emp_id = e.emp_id
--   INNER JOIN branch b ON e.assigned_branch_id = b.branch_id
--   WHERE e.start_date <= date('2004-01-01') AND
--        (e.title = 'Teller' OR e.title = 'Head Teller') AND
--             b.name = 'Woburn Branch'
-- @
--
-- Generated SQL:
--
-- @
--   SELECT ALL T0.account_id AS f0, T0.cust_id AS f1, T0.open_date AS f2,
--   T0.product_cd AS f3 FROM (MAIN.account T0 INNER JOIN MAIN.employee T1
--   ON (T0.open_emp_id = T1.emp_id)) INNER JOIN MAIN.branch T2 ON
--   (T1.assigned_branch_id = T2.branch_id) WHERE ((T1.start_date <=
--   '2004-01-01') AND (((T1.title = 'Teller') OR (T1.title = 'Head
--   Teller')) AND (T2.name = 'Woburn Branch')))
-- @
--
join_5_1_3 :: Relation () Account3
join_5_1_3 = relation $ do
  a <- query account
  e <- query employee
  on $ #openEmpId a .=. just (#empId e)

  b <- query branch
  on $ #assignedBranchId e .=. just (#branchId b)

  wheres $ #startDate e .<=. unsafeSQLiteDayValue "2004-01-01"
  wheres $ #title e .=. just (toFlat $ value "Teller")
     `or'` #title e .=. just (toFlat $ value "Head Teller")
  wheres $ #name b .=. toFlat (value "Woburn Branch")

  return $ Account3 |$| #accountId a
                    |*| #custId a
                    |*| #openDate a
                    |*| #productCd a

data Account3 = Account3
  { a3AccountId :: Int
  , a3CustId :: Int
  , a3OpenDate :: Day
  , a3ProductCd :: String
  } deriving (Show, Generic)

$(makeRelationalRecord ''Account3)

-- | sql/5.3a
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
  on $ #superiorEmpId e .=. just (#empId m)
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
  on $ #superiorEmpId e .=. just (#empId m)
  let emp = #fname e >< #lname e
  let mgr = #fname m >< #lname m
  return $ emp >< mgr

-- | sql/6.4.1a
--
-- The standard SQL allows the syntax of UNION that has an order clause
-- at the last of query. Unfortunately, HRR dows not support. In addition,
-- HRR put a select statement having an order clause into parentheses.
-- If you want to sort whole row returned from UNION, place a order
-- clouse outside of the union relation.
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
--   SELECT ALL T2.f0 AS f0, T2.f1 AS f1 FROM (SELECT ALL T0.emp_id AS f0,
--   T0.assigned_branch_id AS f1 FROM MAIN.employee T0 WHERE (T0.title
--   = 'Teller') UNION SELECT ALL T1.open_emp_id AS f0, T1.open_branch_id
--   AS f1 FROM MAIN.account T1 WHERE (T1.product_cd = 'SAV')) T2 ORDER BY
--   T2.f0 ASC
-- @
--
employee_6_4_1a :: Relation () (Maybe Int, Maybe Int)
employee_6_4_1a = relation $ do
  e  <- query employee
  wheres $ #title e .=. just (toFlat $ value "Teller")
  return $ just (#empId e) >< #assignedBranchId e

account_6_4_1a :: Relation () (Maybe Int, Maybe Int)
account_6_4_1a = relation $ do
  a  <- query account
  wheres $ #productCd a .=. toFlat (value "SAV")
  return $ #openEmpId a >< #openBranchId a

union_6_4_1a_Nest :: Relation () (Maybe Int, Maybe Int)
union_6_4_1a_Nest = relation $ do
  ea <- query $ employee_6_4_1a `union` account_6_4_1a
  asc $ #fst ea
  return ea

-- |
-- Generated SQL has different meaning with the handwritten SQL
-- (order clause with UNION).
-- Such query cannot be expressed flatly with EDSL of HRR.
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
union_6_4_1a_Flat :: Relation () (Maybe Int, Maybe Int)
union_6_4_1a_Flat = relation (do
    e  <- query employee
    wheres $ #title e .=. just (toFlat $ value "Teller")
    return $ just (#empId e) >< #assignedBranchId e
  ) `union` relation (do
    a  <- query account
    wheres $ #productCd a .=. toFlat (value "SAV")
    -- asc $ #openEmpId a
    return $ #openEmpId a >< #openBranchId a
  )

-- | sql/8.1a
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
group_8_1a :: Relation () (Maybe Int, Int64)
group_8_1a = aggregateRelation $ do
  a  <- query account
  g  <- groupBy $ #openEmpId a
  asc $ g
  return $ g >< count (#accountId a)

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
    wheres $ #custId a .=. #custId c
    return (#accountId a)
    ) id' count
  wheres $ toFlat (just (value (2 :: Int64))) .=. ca
  return (customer1 c)

data Customer1 = Customer1
  { c1Custid :: Int
  , c1CustTypeCd :: String
  , c1City :: Maybe String
  } deriving (Show, Generic)

customer1 :: SqlContext c
          => Record c Customer -> Record c Customer1
customer1 c = Customer1 |$| #custId c
                        |*| #custTypeCd c
                        |*| #city c

$(makeRelationalRecord ''Customer1)

-- |
-- (from script) The insert statement
--
-- Handwritten SQL:
--
-- @
--   INSERT INTO branch (branch_id, name, address, city, state, zip)
--   VALUES (null, 'Headquarters', '3882 Main St.', 'Waltham', 'MA', '02451');
-- @
--
-- Literal version of Generated SQL:
--
-- @
--  INSERT INTO MAIN.branch (name, address, city, state, zip)
--  VALUES ('Headquarters', '3882 Main St.', 'Waltham', 'MA', '02451')
-- @
--
insertBranch_s1 :: Insert ()
insertBranch_s1 = insertValueNoPH $ do
  Branch.name'     <-#  value "Headquarters"
  #address  <-#  value (Just "3882 Main St.")
  #city     <-#  value (Just "Waltham")
  #state    <-#  value (Just "MA")
  #zip      <-#  value (Just "02451")

-- |
-- Placeholder version of Generated SQL:
--
-- @
--   INSERT INTO MAIN.branch (name, address, city, state, zip)
--   VALUES (?, ?, ?, ?, ?)
-- @
--
insertBranch_s1P :: Insert Branch1
insertBranch_s1P = insert piBranch1

piBranch1 :: Pi Branch Branch1
piBranch1 = Branch1 |$| #name
                    |*| #address
                    |*| #city
                    |*| #state
                    |*| #zip

data Branch1 = Branch1
  { b1Name :: String
  , b1Address :: Maybe String
  , b1City :: Maybe String
  , b1State :: Maybe String
  , b1Zip :: Maybe String
  } deriving (Generic)

$(makeRelationalRecord ''Branch1)

branch1 :: Branch1
branch1 = Branch1
  { b1Name = "Headquarters"
  , b1Address = Just "3882 Main St."
  , b1City = Just "Waltham"
  , b1State = Just "MA"
  , b1Zip = Just "02451"
  }

-- |
-- Literal version of Generated SQL:
--
-- @
--  INSERT INTO MAIN.branch (name, address, city, state, zip)
--  VALUES ('Headquarters', '3882 Main St.', 'Waltham', 'MA', '02451')
-- @
--
-- Thanks to generic-programing, it is possible to specify record value directly as SQL row value.
-- Above SQL is the same to the monadic building version.
--
insertBranch_s1R :: Insert ()
insertBranch_s1R = insertValueNoPH $ do
  piBranch1   <-#  value Branch1
                         { b1Name = "Headquarters"
                         , b1Address = Just "3882 Main St."
                         , b1City = Just "Waltham"
                         , b1State = Just "MA"
                         , b1Zip = Just "02451"
                         }

-- |
-- Placeholder version of Generated SQL:
--
-- @
--   INSERT INTO MAIN.branch (name, address, city, state, zip)
--   VALUES (?, ?, ?, ?, ?)
-- @
--
-- Thanks to generic-programing, it is possible to specify tuple type as Pi destination type.
-- Above SQL is the same to ad-hoc defined record version.
--
insertBranch_s1PT :: Insert (String, Maybe String, Maybe String, Maybe String, Maybe String)
insertBranch_s1PT = insert piBranchTuple

piBranchTuple :: Pi Branch (String, Maybe String, Maybe String, Maybe String, Maybe String)
piBranchTuple = (,,,,)
                |$| #name
                |*| #address
                |*| #city
                |*| #state
                |*| #zip

branchTuple :: (String, Maybe String, Maybe String, Maybe String, Maybe String)
branchTuple = ("Headquarters",
              Just "3882 Main St.",
              Just "Waltham",
              Just "MA",
              Just "02451")


-- |
-- (from script) The insert statement
--
-- Handwritten SQL:
--
-- @
--   INSERT INTO employee (emp_id, fname, lname, start_date,
--     dept_id, title, assigned_branch_id)
--   VALUES (null, 'Michael', 'Smith', '2001-06-22',
--     (SELECT dept_id FROM department WHERE name = 'Administration'),
--     'President',
--     (SELECT branch_id FROM branch WHERE name = 'Headquarters'));
-- @
--
-- Literal version of Generated SQL:
--
-- @
--   INSERT INTO MAIN.employee (fname, lname, start_date, dept_id, title,
--   assigned_branch_id) SELECT ALL 'Michael' AS f0, 'Smith' AS f1,
--   '2001-06-22' AS f2, T0.dept_id AS f3, 'President' AS f4, T1.branch_id
--   AS f5 FROM MAIN.department T0 INNER JOIN MAIN.branch T1 ON (0=0) WHERE
--   ((T0.name = 'Administration') AND (T1.name = 'Headquarters'))
-- @
--
-- Note: Since the name column of department table is not set with
-- an unique constraint, it is not possible to use queryScalar.
-- The name column of branch table is the same.
--
insertEmployee_s2 :: InsertQuery ()
insertEmployee_s2 = insertQuery piEmployee3 . relation $ do
  d <- query department
  b <- query branch
  wheres $ #name d .=. toFlat (value "Administration")
  wheres $ #name b .=. toFlat (value "Headquarters")
  return $ Employee3 |$| toFlat (value "Michael")
                     |*| toFlat (value "Smith")
                     |*| unsafeSQLiteDayValue "2001-06-22"
                     |*| just (#deptId d)
                     |*| toFlat (value (Just "President"))
                     |*| just (#branchId b)

-- this is equal to `defineDirectPi [1,2,3,6,7,8]'
piEmployee3 :: Pi Employee Employee3
piEmployee3 = Employee3 |$| #fname
                        |*| #lname
                        |*| #startDate
                        |*| #deptId
                        |*| #title
                        |*| #assignedBranchId

data Employee3 = Employee3
  { e3Fname :: String
  , e3Lname :: String
  , e3StartDate :: Day
  , e3DeptId :: Maybe Int
  , e3Title :: Maybe String
  , e3AssignedBranchId :: Maybe Int
  } deriving (Generic)

$(makeRelationalRecord ''Employee3)

-- |
-- In the following code we simulate to use queryScalar with using
-- unsafeUnique. By that means we throw away the safety given by HRR
-- and the type system.
--
-- Unsafe version of Generated SQL:
--
-- @
--   INSERT INTO MAIN.employee (fname, lname, start_date, dept_id, title,
--   assigned_branch_id) SELECT ALL 'Michael' AS f0, 'Smith' AS f1,
--   '2001-06-22' AS f2, (SELECT ALL T0.dept_id AS f0 FROM MAIN.department
--   T0 WHERE (T0.name = 'Administration')) AS f3, 'President' AS f4,
--   (SELECT ALL T1.branch_id AS f0 FROM MAIN.branch T1 WHERE (T1.name
--   = 'Headquarters')) AS f5
-- @
--
insertEmployee_s2U :: InsertQuery ()
insertEmployee_s2U = insertQuery piEmployee3 . relation $ do
  d <- queryScalar . unsafeUnique . relation $ do
    d' <- query department
    wheres $ #name d' .=. toFlat (value "Administration")
    return $ #deptId d'
  b <- queryScalar . unsafeUnique . relation $ do
    b' <- query branch
    wheres $ #name b' .=. toFlat (value "Headquarters")
    return $ #branchId b'
  return $ Employee3 |$| toFlat (value "Michael")
                     |*| toFlat (value "Smith")
                     |*| unsafeSQLiteDayValue "2001-06-22"
                     |*| d
                     |*| toFlat (value (Just "President"))
                     |*| b

-- place the definition of Employee4 that contains template-haskell, before
-- insertEmployee_s2P uses the function to be generated.
data Employee4 = Employee4
  { e4Fname :: String
  , e4Lname :: String
  , e4StartDate :: Day
  , e4Title :: Maybe String
  } deriving (Generic)

$(makeRelationalRecord ''Employee4)

-- |
-- Placeholder version of Generated SQL:
--
-- @
--   INSERT INTO MAIN.employee (fname, lname, start_date, dept_id, title,
--   assigned_branch_id) SELECT ALL ? AS f0, ? AS f1, ? AS f2, T0.dept_id
--   AS f3, ? AS f4, T1.branch_id AS f5 FROM MAIN.department T0 INNER JOIN
--   MAIN.branch T1 ON (0=0) WHERE ((T0.name = 'Administration') AND
--   (T1.name = 'Headquarters'))
-- @
--
insertEmployee_s2P :: InsertQuery Employee4
insertEmployee_s2P = insertQuery piEmployee3 . relation' $ \ph -> do
  d <- query department
  b <- query branch
  wheres $ #name d .=. toFlat (value "Administration")
  wheres $ #name b .=. toFlat (value "Headquarters")
  return $ Employee3 |$| toFlat ((! #e4Fname) ph)
                     |*| toFlat (#e4Lname ph)
                     |*| toFlat (#e4StartDate ph)
                     |*| just (#deptId d)
                     |*| toFlat (#e4Title ph)
                     |*| just (#branchId b)

employee4 :: Employee4
employee4 = Employee4
  { e4Fname = "Michael"
  , e4Lname = "Smith"
  , e4StartDate = read "2001-06-22"
  , e4Title = Just "President"
  }

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
updateEmployee_o3 = updateNoPH $ \proj -> do
  #lname  <-# toFlat (value "Bush")
  #deptId <-# just (toFlat $ value 3)
  wheres $ #empId (proj :: Record Flat Employee) .=. toFlat (value 10)

-- |
-- Placeholder version of Generated SQL:
--
-- @
--   UPDATE MAIN.employee SET lname = ?, dept_id = ? WHERE (emp_id = ?)
-- @
--
-- Note: This function is equal to the following:
--
-- @
--   updateEmployee_o3P :: Update (String, Int, Int)
--   updateEmployee_o3P = derivedUpdate $ \proj -> do
--     (phLname,()) <- placeholder (\ph -> Employee.lname' <-# ph)
--     (phDeptId,()) <- placeholder (\ph -> Employee.deptId' <-# just ph)
--     (phEmpId,()) <- placeholder (\ph -> wheres $ proj ! Employee.empId' .=. ph)
--     return $ (,,) |$| phLname |*| phDeptId |*| phEmpId
-- @
--
updateEmployee_o3P :: Update (String, Int, Int)
updateEmployee_o3P = update $ \proj -> do
  ph <- askPlaceholders
  #lname <-# toFlat (ph ! tuplePi3_0')
  #deptId <-# toFlat (just (ph ! tuplePi3_1'))
  wheres $ #empId (proj :: Record Flat Employee) .=. toFlat (ph ! tuplePi3_2')
  return ()

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
updateAccount_9_4_2 = updateNoPH $ \proj -> do
  ts <- queryScalar $ aggregatedUnique (relation $ do
    t <- query Transaction.transaction
    wheres $ #accountId t .=. #accountId proj
    return (#txnDate t)
    ) id' max'
  tl <- queryList $ relation $ do
    t <- query Transaction.transaction
    wheres $ #accountId t .=. #accountId proj
    return $ toFlat (value (1 :: Int64))
  Account.lastActivityDate' <-# (toDay $ flattenMaybe ts)
  wheres $ exists $ tl

toDay :: SqlContext c => Record c (Maybe LocalTime) -> Record c (Maybe Day)
toDay dt = unsafeProjectSql $ "date(" ++ unsafeShowSql dt ++ ")"

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
deleteAccount_o1 = deleteNoPH $ \proj ->
  wheres $ proj ! Account.accountId' .=. toFlat (value 2)

-- |
-- Placeholder version of Generated SQL:
--
-- @
--   DELETE FROM MAIN.account WHERE (account_id = ?)
-- @
--
-- Note: This function is equal to the following:
--
-- @
--   deleteAccount_o1P :: Delete Int64
--   deleteAccount_o1P = derivedDelete $ \proj -> do
--     fmap fst $ placeholder (\ph -> wheres $ proj ! Account.accountId' .=. ph)
-- @
--
deleteAccount_o1P :: Delete Int
deleteAccount_o1P = delete $ \proj -> do
  ph <- askPlaceholders
  wheres $ proj ! Account.accountId' .=. toFlat ph

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
deleteAccount_o2 = deleteNoPH $ \proj' -> do
  let proj = proj' :: Record Flat Account
  wheres $ #accountId proj .>=. toFlat (value 10)
  wheres $ #accountId proj .<=. toFlat (value 20)

-- |
-- Placeholder version of Generated SQL:
--
-- @
--   DELETE FROM MAIN.account WHERE ((account_id >= ?) AND (account_id <=
--   ?))
-- @
--
deleteAccount_o2P :: Delete (Int, Int)
deleteAccount_o2P = delete $ \proj' -> do
  ph <- askPlaceholders
  let proj = proj' :: Record Flat Account
  wheres $ #accountId proj .>=. toFlat (ph ! fst')
  wheres $ #accountId proj .<=. toFlat (ph ! snd')
  return ()

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
deleteEmployee_9_4_2 = deleteNoPH $ \proj -> do
  el <- queryList $ relation $ do
    e <- query employee
    wheres $ #deptId e .=. just (#deptId (proj :: Record Flat Department))
    return $ toFlat (value (1 :: Int64))
  wheres $ not' . exists $ el

--
-- run and print sql
--

run :: (Show a, IConnection conn, FromSql SqlValue a, ToSql SqlValue p)
    => conn -> p -> Relation p a -> IO ()
run conn param rel = do
  putStrLn $ "SQL: " ++ show rel
  records <- runRelation conn rel param
  mapM_ print records
  putStrLn ""

runI :: (IConnection conn, ToSql SqlValue p)
     => conn -> p -> Insert p -> IO ()
runI conn param ins = do
  putStrLn $ "SQL: " ++ show ins
  num <- runInsert conn ins param
  print num
  putStrLn ""
  rollback conn

runIQ :: (IConnection conn, ToSql SqlValue p)
     => conn -> p -> InsertQuery p -> IO ()
runIQ conn param ins = do
  putStrLn $ "SQL: " ++ show ins
  num <- runInsertQuery conn ins param
  print num
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
main = handleSqlError' $ withConnectionIO (connectSqlite3 "examples.db") $ \conn -> do
  run conn () allAccount
  run conn () account_3_7
  run conn () account_3_7_1
  run conn () employee_3_7_3
  run conn () employee_4_1_2
  run conn (read "2003-01-01") employee_4_1_2P
  run conn () employee_4_3_2
  run conn (read "2001-01-01", read "2003-01-01") employee_4_3_2P
  run conn () account_4_3_3a
  run conn () account_4_3_3aT
  run conn () account_4_3_3aR
  run conn () account_9_1
  run conn "ACCOUNT" account_4_3_3b
  run conn "ACCOUNT" account_4_3_3bT
  run conn "ACCOUNT" account_4_3_3bR
  run conn () account_4_3_3c
  run conn () join_5_1_2a
  run conn () join_5_1_2aT
  run conn () account_LeftOuterJoin
  putStrLn $ "SQL: " ++ show business_RightOuterJoin -- right join is not supported by SQLite3
  run conn () join_5_1_3
  run conn () selfJoin_5_3a
  run conn () selfJoin_5_3aT
  run conn () union_6_4_1a_Nest
  run conn () union_6_4_1a_Flat
  run conn () group_8_1a
  run conn () customer_9_4
  runI conn () insertBranch_s1
  runI conn branch1 insertBranch_s1P
  runI conn () insertBranch_s1R
  runI conn branchTuple insertBranch_s1PT
  runIQ conn () insertEmployee_s2
  runIQ conn () insertEmployee_s2U
  runIQ conn employee4 insertEmployee_s2P
  runU conn () updateEmployee_o3
  runU conn ("Bush", 3, 10) updateEmployee_o3P
  runU conn () updateAccount_9_4_2
  runD conn () deleteAccount_o1
  runD conn 2 deleteAccount_o1P
  runD conn () deleteAccount_o2
  runD conn (10,20) deleteAccount_o2P
  runD conn () deleteEmployee_9_4_2

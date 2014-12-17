{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

import Database.Record

import Database.Relational.Query
import Database.HDBC (IConnection, SqlValue)
import Database.HDBC.Query.TH (makeRecordPersistableDefault)
import Data.Int (Int64)
import Data.Time (Day)

import qualified Account
import Account (Account, account)
--import qualified Customer
--import Customer (Customer, customer)
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
--import qualified Transaction
--import Transaction (Transaction, transaction)
--import qualified Business
--import Business (Business, business)
import qualified Department
import Department (Department, department)
--import qualified Product
--import Product (Product, product)
import qualified Employee
import Employee (Employee, employee)

import DataSource (connect)
import Database.HDBC.Record.Query (runQuery)
import Database.HDBC.Session (withConnectionIO, handleSqlError')

import Prelude hiding (product)

allAccount :: Relation () Account
allAccount = relation $ query account

-- sql/4.3.3a.sh
--
-- @
--   SELECT account_id, product_cd, cust_id, avail_balance
--   FROM LEARNINGSQL.account
--   WHERE product_cd IN ('CHK', 'SAV', 'CD', 'MM')
-- @
--
account_4_3_3a :: Relation () Account
account_4_3_3a = relation $ do
  a  <- query account
  wheres $ a ! Account.productCd' `in'` values ["CHK", "SAV", "CD", "MM"]
  return a

account_4_3_3aT :: Relation () (((Int64, String), Int64), Maybe Double)
account_4_3_3aT = relation $ do
  a  <- query account
  wheres $ a ! Account.productCd' `in'` values ["CHK", "SAV", "CD", "MM"]
  return $ a ! Account.accountId' >< a ! Account.productCd' >< a ! Account.custId' >< a ! Account.availBalance'

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
-- @
--   SELECT e.fname, e.lname, d.name
--   FROM LEARNINGSQL.employee e INNER JOIN LEARNINGSQL.department d
--   USING (dept_id)
-- @
--
join_5_1_2a :: Relation () (Employee, Department)
join_5_1_2a = relation $ do
  e  <- query employee
  d  <- query department
  on $ e ! Employee.deptId' .=. just (d ! Department.deptId')
  return $ e >< d

join_5_1_2aT :: Relation () ((String, String), String)
join_5_1_2aT = relation $ do
  e  <- query employee
  d  <- query department
  on $ e ! Employee.deptId' .=. just (d ! Department.deptId')
  return $ e ! Employee.fname' >< e ! Employee.lname' >< d ! Department.name'

-- | sql/5.3a.sh
--
-- @
--   SELECT e.fname, e.lname, e_mgr.fname mgr_fname, e_mgr.lname mgr_lname
--   FROM LEARNINGSQL.employee e INNER JOIN LEARNINGSQL.employee e_mgr
--   ON e.superior_emp_id = e_mgr.emp_id
-- @
--
selfJoin_5_3a :: Relation () (Employee, Employee)
selfJoin_5_3a = relation $ do
  e  <- query employee
  m  <- query employee
  on $ e ! Employee.superiorEmpId' .=. just (m ! Employee.empId')
  return $ e >< m

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

union_6_4_1a_Nest :: Relation () (Maybe Int64, Maybe Int64)
union_6_4_1a_Nest = relation $ do
  ea <- query $ employee_6_4_1a `union` account_6_4_1a
  asc $ ea ! fst'
  return ea

union_6_4_1a_Flat :: Relation () (Maybe Int64, Maybe Int64)
union_6_4_1a_Flat = relation (do
    e  <- query employee
    wheres $ e ! Employee.title' .=. just (value "Teller")
    return $ just (e ! Employee.empId') >< e ! Employee.assignedBranchId'
  ) `union` relation (do
    a  <- query account
    wheres $ a ! Account.productCd' .=. value "SAV"
    return $ a ! Account.openEmpId' >< a ! Account.openBranchId'
  )

-- | sql/8.1a.sh
--
-- @
--   SELECT open_emp_id, COUNT(*) how_many
--   FROM LEARNINGSQL.account
--   GROUP BY open_emp_id
--   ORDER BY open_emp_id
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
-- @
--   SELECT account_id, product_cd, cust_id, avail_balance
--   FROM account
--   WHERE product_cd IN (SELECT product_cd FROM product
--   WHERE product_type_cd = 'ACCOUNT')
-- @

product_4_3_3b :: Relation String String
product_4_3_3b = relation' $ do
  p <- query product
  (phProductCd,()) <- placeholder (\ph -> wheres $ p ! Product.productTypeCd' .=. ph)
  let productCd = p ! Product.productCd'
  return (phProductCd, productCd)

account_4_3_3b :: Relation String Account
account_4_3_3b = relation' $ do
  a <- query account
  (phProductCd,p) <- queryList' product_4_3_3b
  wheres $ a ! Account.productCd' `in'` p
  return (phProductCd, a)

account_4_3_3bT :: Relation String (((Int64, String), Int64), Maybe Double)
account_4_3_3bT = relation' $ do
  a <- query account
  (phProductCd,p) <- queryList' product_4_3_3b
  wheres $ a ! Account.productCd' `in'` p
  let at = a ! Account.accountId' >< a ! Account.productCd' >< a ! Account.custId' >< a ! Account.availBalance'
  return (phProductCd, at)

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

-- | sql/4.3.3c.sh
--
-- @
--   SELECT account_id, product_cd, cust_id, avail_balance
--   FROM LEARNINGSQL.account
--   WHERE product_cd NOT IN ('CHK', 'SAV', 'CD', 'MM')
-- @
--
account_4_3_3c :: Relation () Account
account_4_3_3c = relation $ do
  a  <- query account
  wheres $ not' (a ! Account.productCd' `in'` values ["CHK", "SAV", "CD", "MM"])
  return a

-- | sql/3.7
--
-- @
--   SELECT open_emp_id, product_cd
--   FROM account
--   ORDER BY open_emp_id, product_cd
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
-- @
--   SELECT account_id, product_cd, open_date, avail_balance
--   FROM account
--   ORDER BY avail_balance DESC
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
-- @
--   SELECT emp_id, title, start_date, fname, lname
--   FROM employee
--   ORDER BY 2,5
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
-- @
--   SELECT *
--   FROM employee
--   WHERE end_date IS NULL AND (title = 'Teller' OR start_date < '2003-01-01')
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
-- @
--   SELECT emp_id, fname, lname, start_date FROM employee
--   WHERE start_date
--   BETWEEN date('2001-01-01') AND date('2002-12-31')
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


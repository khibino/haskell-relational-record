{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE FlexibleContexts #-}

import Database.Record

import Database.Relational.Query
import Database.HDBC (IConnection, SqlValue)
import Data.Int (Int32, Int64)

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
import Database.HDBC.Record.Query (runQuery)
import Database.HDBC.Session (withConnectionIO, handleSqlError')

allAccount :: Relation () Account
allAccount =
  relation
  [ a
  | a  <- query account
  ]

-- sql/4.3.3a.sh
--
-- @
--   SELECT account_id, product_cd, cust_id, avail_balance
--   FROM LEARNINGSQL.account
--   WHERE product_cd IN ('CHK', 'SAV', 'CD', 'MM')
-- @
--
account1 :: Relation () Account
account1 =
  relation
  [ a
  | a  <- query account
  , () <- wheres $ a ! Account.productCd' `in'` values ["CHK", "SAV", "CD", "MM"]
  ]

account1' :: Relation () (((Int32, String), Int32), Maybe Double)
account1' =
  relation
  [ a ! Account.accountId' >< a ! Account.productCd' >< a ! Account.custId' >< a ! Account.availBalance'
  | a  <- query account
  , () <- wheres $ a ! Account.productCd' `in'` values ["CHK", "SAV", "CD", "MM"]
  ]

-- | sql/5.1.2a.sh
--
-- @
--   SELECT e.fname, e.lname, d.name
--   FROM LEARNINGSQL.employee e INNER JOIN LEARNINGSQL.department d
--   USING (dept_id)
-- @
--
join1 :: Relation () (Employee, Department)
join1 =
  relation
  [ e >< d
  | e  <- query employee
  , d  <- query department
  , () <- on $ e ! Employee.deptId' .=. just (d ! Department.deptId')
  ]

join1' :: Relation () ((String, String), String)
join1' =
  relation
  [ e ! Employee.fname' >< e ! Employee.lname' >< d ! Department.name'
  | e  <- query employee
  , d  <- query department
  , () <- on $ e ! Employee.deptId' .=. just (d ! Department.deptId')
  ]

-- | sql/5.3a.sh
--
-- @
--   SELECT e.fname, e.lname, e_mgr.fname mgr_fname, e_mgr.lname mgr_lname
--   FROM LEARNINGSQL.employee e INNER JOIN LEARNINGSQL.employee e_mgr
--   ON e.superior_emp_id = e_mgr.emp_id
-- @
--
selfJoin1 :: Relation () (Employee, Employee)
selfJoin1 =
  relation
  [ e >< m
  | e  <- query employee
  , m  <- query employee
  , () <- on $ e ! Employee.superiorEmpId' .=. just (m ! Employee.empId')
  ]

selfJoin1' :: Relation () ((String, String), (String, String))
selfJoin1' =
  relation
  [ emp >< mgr
  | e  <- query employee
  , m  <- query employee
  , () <- on $ e ! Employee.superiorEmpId' .=. just (m ! Employee.empId')
  , let emp = e ! Employee.fname' >< e ! Employee.lname'
  , let mgr = m ! Employee.fname' >< m ! Employee.lname'
  ]

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
employee1 :: Relation () (Maybe Int32, Maybe Int32)
employee1 =
  relation
  [ just (e ! Employee.empId') >< e ! Employee.assignedBranchId'
  | e  <- query employee
  , () <- wheres $ e ! Employee.title' .=. just (value "Teller")
  ]

account2 :: Relation () (Maybe Int32, Maybe Int32)
account2 =
  relation
  [ a ! Account.openEmpId' >< a ! Account.openBranchId'
  | a  <- query account
  , () <- wheres $ a ! Account.productCd' .=. value "SAV"
  ]

union1 :: Relation () (Maybe Int32, Maybe Int32)
union1 =
  relation
  [ ea
  | ea <- query $ employee1 `union` account2
  , () <- asc $ ea ! fst'
  ]

union1' :: Relation () (Maybe Int32, Maybe Int32)
union1' =
  relation
  [ ea
  | ea <- query $ relation
          [ just (e ! Employee.empId') >< e ! Employee.assignedBranchId'
          | e  <- query employee
          , () <- wheres $ e ! Employee.title' .=. just (value "Teller")
          ]
          `union` relation
          [ a ! Account.openEmpId' >< a ! Account.openBranchId'
          | a  <- query account
          , () <- wheres $ a ! Account.productCd' .=. value "SAV"
          ]
  , () <- asc $ ea ! fst'
  ]

-- | sql/8.1a.sh
--
-- @
--   SELECT open_emp_id, COUNT(*) how_many
--   FROM LEARNINGSQL.account
--   GROUP BY open_emp_id
--   ORDER BY open_emp_id
-- @
--
group1 :: Relation () (Maybe Int32, Int64)
group1 =
  aggregateRelation
  [ g >< count a
  | a  <- query account
  , g  <- groupBy $ a ! Account.openEmpId'
  , () <- asc $ g ! id'
  ]

runAndPrint :: (Show a, IConnection conn, FromSql SqlValue a, ToSql SqlValue p)
            => conn -> Relation p a -> p -> IO ()
runAndPrint conn rel param = do
  putStrLn $ "SQL: " ++ show rel
  records <- runQuery conn (relationalQuery rel) param
  mapM_ print records
  putStrLn ""

main :: IO ()
main = handleSqlError' $ withConnectionIO connect $ \conn -> do
  let run :: (Show a, FromSql SqlValue a, ToSql SqlValue p) => Relation p a -> p -> IO ()
      run = runAndPrint conn
  run allAccount ()
  run account1 ()
  run account1' ()
  run join1 ()
  run join1' ()
  run selfJoin1 ()
  run selfJoin1' ()
  run union1 ()
  run union1' ()
  run group1 ()

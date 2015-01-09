---
layout: default
title: Examples
---

### Preparing

We assume that you have read both [quick start](quickstart.html) and [tutorial](tutorial.htm).

### Schema of examples

We use the bank example in [Learning SQL](http://shop.oreilly.com/product/9780596007270.do). Its support page provides a script to create the tables of the bank examples for MySQL. We modified [it for SQLite](https://github.com/khibino/haskell-relational-record/blob/master/relational-record-examples/sql/add.sql) and created a DB file called "examples.db" in the top directory of "relational-record-examples". We deeply thank Alan Beaulieu, the author of "Learning SQL".

Here is a list of tables copied from page 34 of "Learning SQL":

- Account -- a particular product opened for a particular customer
- Business -- a corporate customer (subtype of the Customer table)
- Customer -- a person or corporation known to the bank
- Department -- a group of bank employees implementing a particular banking function
- Employee -- a person working for the bank
- Individual -- a noncorporate customer (subtype of the Customer table)
- Officer -- a person allowed to transact business for a corporate customer
- Product -- a banking function offered to customers
- Product_type -- a group of products having similar function
- Transaction -- a change made to an account balance

The most of the following examples come from "Learning SQL", too.
HRR code examples are found in "src/examples.hs".

### Select

#### Descending sort order

SQL:

{% highlight sql %}
SELECT account_id, product_cd, open_date, avail_balance
FROM account
ORDER BY avail_balance DESC;
{% endhighlight %}

HRR:

{% highlight haskell %}
account_3_7_1 :: Relation () Account
account_3_7_1 = relation $ do
  a <- query account
  desc $ a ! Account.availBalance'
  return a
{% endhighlight %}

Generated SQL:

{% highlight sql %}
SELECT ALL T0.account_id AS f0,
           T0.product_cd AS f1,
           T0.open_date AS f2,
           T0.avail_balance AS f3
FROM MAIN.account T0
ORDER BY T0.avail_balance DESC
{% endhighlight %}

#### The order by clause

SQL:

{% highlight sql %}
SELECT open_emp_id, product_cd
FROM account
ORDER BY open_emp_id, product_cd;
{% endhighlight %}

HRR:

{% highlight haskell %}
account_3_7 :: Relation () (Maybe Int64, String)
account_3_7 = relation $ do
  a <- query account
  let proj = (,) |$| a ! Account.openEmpId'
                 |*| a ! Account.productCd'
  asc proj
  return proj
{% endhighlight %}

Generated SQL:

{% highlight sql %}
SELECT ALL T0.open_emp_id AS f0,
           T0.product_cd AS f1
FROM MAIN.account T0
ORDER BY T0.open_emp_id ASC, T0.product_cd ASC
{% endhighlight %}

#### Sorting via numeric placeholders

For backwards compatibility with the SQL92 version of standard, you can use numbers instead of names to specify the columns that should be sorted. With HRR you cannot use numbers for such purpose.

SQL:

{% highlight sql %}
SELECT emp_id, title, start_date, fname, lname
FROM employee
ORDER BY 2,5;
{% endhighlight %}

HRR: constructing new records in Applicative-like style.

{% highlight haskell %}
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
{% endhighlight %}

Generated SQL:

{% highlight sql %}
SELECT ALL T0.emp_id AS f0,
           T0.title AS f1,
           T0.start_date AS f2,
           T0.fname AS f3,
           T0.lname AS f4
FROM MAIN.employee T0
ORDER BY T0.title ASC, T0.lname ASC
{% endhighlight %}

#### Using the is null operator and the date literal

HRR supports date literal of the SQL standard, such like Date '2003-01-01'. However, SQLite has its own date literal without Date keyword, like this: '2003-01-01'. So, you have to define a function to support SQLite's date literal. Here we define `unsafeSQLiteDayValue` function for that.

SQL:

{% highlight sql %}
SELECT *
FROM employee
WHERE end_date IS NULL AND (title = 'Teller' OR start_date < '2003-01-01');
{% endhighlight %}

HRR:

{% highlight haskell %}
employee_4_1_2 :: Relation () Employee
employee_4_1_2 = relation $ do
  e <- query employee
  wheres $ isNothing (e ! Employee.endDate')
  wheres $ e ! Employee.title' .=. just (value "Teller")
     `or'` e ! Employee.startDate' .<. unsafeSQLiteDayValue "2003-01-01"
  return e

unsafeSQLiteDayValue :: SqlProjectable p => String -> p Day
unsafeSQLiteDayValue = unsafeProjectSqlTerms . showConstantTermsSQL
{% endhighlight %}

Generated SQL:

{% highlight sql %}
SELECT ALL T0.emp_id AS f0,
           T0.fname AS f1,
           T0.lname AS f2,
           T0.start_date AS f3,
           T0.end_date AS f4,
           T0.superior_emp_id AS f5,
           T0.dept_id AS f6,
           T0.title AS f7,
           T0.assigned_branch_id AS f8
FROM MAIN.employee T0
WHERE ((T0.end_date IS NULL) AND ((T0.title = 'Teller') OR (T0.start_date < '2003-01-01')))
{% endhighlight %}

Another way, use a placeholder instead of a date literal. There is no need to define a helper function:

{% highlight haskell %}
employee_4_1_2P :: Relation Day Employee
employee_4_1_2P = relation' $ do
  e <- query employee
  wheres $ isNothing (e ! Employee.endDate')
  (phDay,()) <- placeholder (\ph ->
    wheres $ e ! Employee.title' .=. just (value "Teller")
       `or'` e ! Employee.startDate' .<. ph)
  return (phDay, e)
{% endhighlight %}

Generated SQL:

{% highlight sql %}
SELECT ALL T0.emp_id AS f0,
           T0.fname AS f1,
           T0.lname AS f2,
           T0.start_date AS f3,
           T0.end_date AS f4,
           T0.superior_emp_id AS f5,
           T0.dept_id AS f6,
           T0.title AS f7,
           T0.assigned_branch_id AS f8
FROM MAIN.employee T0
WHERE ((T0.end_date IS NULL) AND ((T0.title = 'Teller') OR (T0.start_date < ?)))
{% endhighlight %}

#### Range condition with the between operator

SQL:

{% highlight sql %}
SELECT emp_id, fname, lname, start_date FROM employee
WHERE start_date
BETWEEN date('2001-01-01') AND date('2002-12-31');
{% endhighlight %}

HRR:

{% highlight haskell %}
employee_4_3_2 :: Relation () Employee2
employee_4_3_2 = relation $ do
  e <- query employee
  wheres $ e ! Employee.startDate' .>=. unsafeSQLiteDayValue "2001-01-01"
  wheres $ e ! Employee.startDate' .<=. unsafeSQLiteDayValue "2003-01-01"
  return $ Employee2 |$| e ! Employee.empId'
                     |*| e ! Employee.fname'
                     |*| e ! Employee.lname'
                     |*| e ! Employee.startDate'

data Employee2 = Employee2
  { e2EmpId :: Int64
  , e2Fname :: String
  , e2Lname :: String
  , e2StartDate :: Day
  } deriving (Show)

$(makeRecordPersistableDefault ''Employee2)
{% endhighlight %}

Generated SQL:

{% highlight sql %}
SELECT ALL T0.emp_id AS f0,
           T0.fname AS f1,
           T0.lname AS f2,
           T0.start_date AS f3
FROM MAIN.employee T0
WHERE ((T0.start_date >= '2001-01-01') AND (T0.start_date <= '2003-01-01'))
{% endhighlight %}

HRR with place holder:

{% highlight haskell %}
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
{% endhighlight %}

Generated SQL:

{% highlight sql %}
SELECT ALL T0.emp_id AS f0,
           T0.fname AS f1,
           T0.lname AS f2,
           T0.start_date AS f3
FROM MAIN.employee T0
WHERE ((T0.start_date >= ?) AND (T0.start_date <= ?))
{% endhighlight %}

#### Membership conditions

SQL:

{% highlight sql %}
SELECT account_id, product_cd, cust_id, avail_balance
FROM account
WHERE product_cd IN ('CHK', 'SAV', 'CD', 'MM');
{% endhighlight %}

HRR: returning raw rows.

{% highlight haskell %}
account_4_3_3a :: Relation () Account
account_4_3_3a = relation $ do
  a <- query account
  wheres $ a ! Account.productCd' `in'` values ["CHK", "SAV", "CD", "MM"]
  return a
{% endhighlight %}

Generated SQL:

{% highlight sql %}
SELECT ALL T0.account_id AS f0,
           T0.product_cd AS f1,
           T0.cust_id AS f2,
           T0.open_date AS f3,
           T0.close_date AS f4,
           T0.last_activity_date AS f5,
           T0.status AS f6,
           T0.open_branch_id AS f7,
           T0.open_emp_id AS f8,
           T0.avail_balance AS f9,
           T0.pending_balance AS f10
FROM MAIN.account T0
WHERE (T0.product_cd IN ('CHK', 'SAV', 'CD', 'MM'))
{% endhighlight %}

HRR: constructing new records in Applicative-like style.

{% highlight haskell %}
data Account1 = Account1
  { a1AccountId :: Int64
  , a1ProductCd :: String
  , a1CustId :: Int64
  , a1AvailBalance :: Maybe Double
  } deriving (Show)

$(makeRecordPersistableDefault ''Account1)

account_4_3_3aR :: Relation () Account1
account_4_3_3aR = relation $ do
  a  <- query account
  wheres $ a ! Account.productCd' `in'` values ["CHK", "SAV", "CD", "MM"]
  return $ Account1 |$| a ! Account.accountId'
                    |*| a ! Account.productCd'
                    |*| a ! Account.custId'
                    |*| a ! Account.availBalance'
{% endhighlight %}

Generated SQL:

{% highlight sql %}
SELECT ALL T0.account_id AS f0,
           T0.product_cd AS f1,
           T0.cust_id AS f2,
           T0.avail_balance AS f3
FROM MAIN.account T0
WHERE (T0.product_cd IN ('CHK', 'SAV', 'CD', 'MM'))
{% endhighlight %}

#### Subquery

SQL:
{% highlight sql %}
SELECT account_id, product_cd, cust_id, avail_balance
FROM account
WHERE account_id = (SELECT MAX(account_id)
                    FROM account);
{% endhighlight %}

HRR:

{% highlight haskell %}
account_9_1 :: Relation () Account1
account_9_1 = relation $ do
  a  <- query account
  ma <- queryScalar $ aggregatedUnique account Account.accountId' max'
  wheres $ just (a ! Account.accountId') .=. flattenMaybe ma
  return $ Account1 |$| a ! Account.accountId'
                    |*| a ! Account.productCd'
                    |*| a ! Account.custId'
                    |*| a ! Account.availBalance'
{% endhighlight %}

Generated SQL:

{% highlight sql %}
SELECT ALL T0.account_id AS f0,
           T0.product_cd AS f1,
           T0.cust_id AS f2,
           T0.avail_balance AS f3
FROM MAIN.account T0
WHERE (T0.account_id = (SELECT ALL MAX (T1.account_id) AS f0
                        FROM MAIN.account T1))
{% endhighlight %}

#### Membership conditions using subqueries

SQL:

{% highlight sql %}
SELECT account_id, product_cd, cust_id, avail_balance
FROM account
WHERE product_cd IN (SELECT product_cd
                     FROM product
                     WHERE product_type_cd = 'ACCOUNT');
{% endhighlight %}

HRR:

{% highlight haskell %}
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
{% endhighlight %}

Using type holders:

{% highlight haskell %}
run conn "ACCOUNT" account_4_3_3bR
{% endhighlight %}

Generated SQL:

{% highlight sql %}
SELECT ALL T0.account_id AS f0,
           T0.product_cd AS f1,
           T0.cust_id AS f2,
           T0.open_date AS f3,
           T0.close_date AS f4,
           T0.last_activity_date AS f5,
           T0.status AS f6,
           T0.open_branch_id AS f7,
           T0.open_emp_id AS f8,
           T0.avail_balance AS f9,
           T0.pending_balance AS f10
FROM MAIN.account T0
WHERE (T0.product_cd IN (SELECT ALL T1.product_cd AS f0
                         FROM MAIN.product T1
                         WHERE (T1.product_type_cd = ?)))
{% endhighlight %}

#### Membership conditions using not in

SQL:

{% highlight sql %}
SELECT account_id, product_cd, cust_id, avail_balance
FROM account
WHERE product_cd NOT IN ('CHK', 'SAV', 'CD', 'MM');
{% endhighlight %}

HRR:

{% highlight haskell %}
account_4_3_3c :: Relation () Account
account_4_3_3c = relation $ do
  a  <- query account
  wheres $ not' (a ! Account.productCd' `in'` values ["CHK", "SAV", "CD", "MM"])
  return a
{% endhighlight %}

Generated SQL:

{% highlight sql %}
SELECT ALL T0.account_id AS f0,
           T0.product_cd AS f1,
           T0.cust_id AS f2,
           T0.open_date AS f3,
           T0.close_date AS f4,
           T0.last_activity_date AS f5,
           T0.status AS f6,
           T0.open_branch_id AS f7,
           T0.open_emp_id AS f8,
           T0.avail_balance AS f9,
           T0.pending_balance AS f10
FROM MAIN.account T0
WHERE (NOT (T0.product_cd IN ('CHK', 'SAV', 'CD', 'MM')))
{% endhighlight %}

#### Inner join

SQL:

{% highlight sql %}
SELECT e.fname, e.lname, d.name
FROM employee e INNER JOIN department d
USING (dept_id);
{% endhighlight %}

HRR:

{% highlight haskell %}
join_5_1_2aT :: Relation () ((String, String), String)
join_5_1_2aT = relation $ do
  e <- query employee
  d <- query department
  on $ e ! Employee.deptId' .=. just (d ! Department.deptId')
  return $ e ! Employee.fname' >< e ! Employee.lname' >< d ! Department.name'
{% endhighlight %}

Generated SQL:

{% highlight sql %}
SELECT ALL T0.fname AS f0,
           T0.lname AS f1,
           T1.name AS f2
FROM MAIN.employee T0 INNER JOIN MAIN.department T1
ON (T0.dept_id = T1.dept_id)
{% endhighlight %}

#### Complex join

SQL:

{% highlight sql %}
SELECT a.account_id, a.cust_id, a.open_date, a.product_cd
FROM account a INNER JOIN employee e ON a.open_emp_id = e.emp_id
INNER JOIN branch b ON e.assigned_branch_id = b.branch_id
WHERE e.start_date <= date('2004-01-01') AND
     (e.title = 'Teller' OR e.title = 'Head Teller') AND
     b.name = 'Woburn Branch';
{% endhighlight %}

HRR: TBD

#### Self-join

SQL:

{% highlight sql %}
SELECT e.fname, e.lname, e_mgr.fname mgr_fname, e_mgr.lname mgr_lname
FROM employee e INNER JOIN employee e_mgr
ON e.superior_emp_id = e_mgr.emp_id
{% endhighlight %}

HRR:

{% highlight haskell %}
selfJoin_5_3aT :: Relation () ((String, String), (String, String))
selfJoin_5_3aT = relation $ do
  e <- query employee
  m <- query employee
  on $ e ! Employee.superiorEmpId' .=. just (m ! Employee.empId')
  let emp = e ! Employee.fname' >< e ! Employee.lname'
  let mgr = m ! Employee.fname' >< m ! Employee.lname'
  return $ emp >< mgr
{% endhighlight %}

Generated SQL:

{% highlight sql %}
SELECT ALL T0.fname AS f0,
           T0.lname AS f1,
           T1.fname AS f2,
           T1.lname AS f3
FROM MAIN.employee T0 INNER JOIN MAIN.employee T1
ON (T0.superior_emp_id = T1.emp_id)
{% endhighlight %}

####Sorting compound query results

SQL:

{% highlight sql %}
SELECT emp_id, assigned_branch_id
FROM employee
WHERE title = 'Teller'
UNION
SELECT open_emp_id, open_branch_id
FROM account
WHERE product_cd = 'SAV'
ORDER BY emp_id;
{% endhighlight %}

HRR:

{% highlight haskell %}
employee_6_4_1a :: Relation () (Maybe Int64, Maybe Int64)
employee_6_4_1a = relation $ do
  e <- query employee
  wheres $ e ! Employee.title' .=. just (value "Teller")
  return $ just (e ! Employee.empId') >< e ! Employee.assignedBranchId'

account_6_4_1a :: Relation () (Maybe Int64, Maybe Int64)
account_6_4_1a = relation $ do
  a <- query account
  wheres $ a ! Account.productCd' .=. value "SAV"
  return $ a ! Account.openEmpId' >< a ! Account.openBranchId'

union_6_4_1a_Nest :: Relation () (Maybe Int64, Maybe Int64)
union_6_4_1a_Nest = relation $ do
  ea <- query $ employee_6_4_1a `union` account_6_4_1a
  asc $ ea ! fst'
  return ea
{% endhighlight %}

Generated SQL:

{% highlight sql %}
SELECT ALL T2.f0 AS f0,
           T2.f1 AS f1
FROM (SELECT ALL T0.emp_id AS f0,
                 T0.assigned_branch_id AS f1
      FROM MAIN.employee T0
      WHERE (T0.title = 'Teller')
      UNION
      SELECT ALL T1.open_emp_id AS f0,
                 T1.open_branch_id AS f1
      FROM MAIN.account T1
      WHERE (T1.product_cd = 'SAV')) T2
ORDER BY T2.f0 ASC
{% endhighlight %}

HRR:

{% highlight haskell %}
union_6_4_1a_Flat :: Relation () (Maybe Int64, Maybe Int64)
union_6_4_1a_Flat = relation (do
    e <- query employee
    wheres $ e ! Employee.title' .=. just (value "Teller")
    return $ just (e ! Employee.empId') >< e ! Employee.assignedBranchId'
  ) `union` relation (do
    a <- query account
    wheres $ a ! Account.productCd' .=. value "SAV"
    return $ a ! Account.openEmpId' >< a ! Account.openBranchId'
  )
{% endhighlight %}

Generated SQL:

{% highlight sql %}
SELECT ALL T0.emp_id AS f0,
           T0.assigned_branch_id AS f1
FROM MAIN.employee T0
WHERE (T0.title = 'Teller')
UNION
SELECT ALL T1.open_emp_id AS f0,
           T1.open_branch_id AS f1
FROM MAIN.account T1
WHERE (T1.product_cd = 'SAV')
{% endhighlight %}

#### Grouping

SQL:

{% highlight sql %}
SELECT open_emp_id, COUNT(*) how_many
FROM account
GROUP BY open_emp_id
ORDER BY open_emp_id;
{% endhighlight %}

HRR:

{% highlight haskell %}
group_8_1a :: Relation () (Maybe Int64, Int64)
group_8_1a = aggregateRelation $ do
  a <- query account
  g <- groupBy $ a ! Account.openEmpId'
  asc $ g ! id'
  return $ g >< count (a ! Account.accountId')
{% endhighlight %}

Generated SQL:

{% highlight sql %}
SELECT ALL T0.open_emp_id AS f0,
           COUNT (T0.account_id) AS f1
FROM MAIN.account T0
GROUP BY T0.open_emp_id
ORDER BY T0.open_emp_id ASC
{% endhighlight %}

#### Correlated Subqueries

SQL:
{% highlight sql %}
SELECT c.cust_id, c.cust_type_cd, c.city
FROM customer c
WHERE 2 = (SELECT COUNT(*)
           FROM account a
           WHERE a.cust_id = c.cust_id);
{% endhighlight %}

HRR:

{% highlight haskell %}
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
{% endhighlight %}

Generated SQL:

{% highlight sql %}
SELECT ALL T0.cust_id AS f0,
           T0.cust_type_cd AS f1,
           T0.city AS f2
FROM MAIN.customer T0
WHERE (2 = (SELECT ALL COUNT (T2.f0) AS f0
            FROM (SELECT ALL T1.account_id AS f0
                  FROM MAIN.account T1
                  WHERE (T1.cust_id = T0.cust_id)) T2))
{% endhighlight %}

### insert

#### Inserting data

SQL:

{% highlight sql %}
INSERT INTO branch (branch_id, name, address, city, state, zip)
VALUES (null, 'Headquarters', '3882 Main St.', 'Waltham', 'MA', '02451');
{% endhighlight %}

HRR:

{% highlight haskell %}
insertBranch_s1 :: InsertQuery ()
insertBranch_s1 = typedInsertQuery piBranch1 $ relation .
  return $ Branch1 |$| value "Headquarters"
                   |*| value (Just "3882 Main St.")
                   |*| value (Just "Waltham")
                   |*| value (Just "MA")
                   |*| value (Just "02451")

-- this is equal to `definePi 1'
piBranch1 :: Pi Branch Branch1
piBranch1 = Branch1 |$| Branch.name'
                    |*| Branch.address'
                    |*| Branch.city'
                    |*| Branch.state'
                    |*| Branch.zip'

branch1 :: Branch1
branch1 = Branch1
  { b1Name = "Headquarters"
  , b1Address = Just "3882 Main St."
  , b1City = Just "Waltham"
  , b1State = Just "MA"
  , b1Zip = Just "02451"
  }

data Branch1 = Branch1
  { b1Name :: String
  , b1Address :: Maybe String
  , b1City :: Maybe String
  , b1State :: Maybe String
  , b1Zip :: Maybe String
  }

$(makeRecordPersistableDefault ''Branch1)
{% endhighlight %}

Generated SQL:

{% highlight sql %}
INSERT INTO MAIN.branch (name, address, city, state, zip)
SELECT ALL 'Headquarters' AS f0,
           '3882 Main St.' AS f1,
           'Waltham' AS f2, 
           'MA' AS f3, '02451' AS f4
{% endhighlight %}

HRR using place holder:

{% highlight haskell %}
insertBranch_s1P :: Insert Branch1
insertBranch_s1P = typedInsert tableOfBranch piBranch1
{% endhighlight %}

Generated SQL:

{% highlight sql %}
INSERT INTO MAIN.branch (name, address, city, state, zip)
VALUES (?, ?, ?, ?, ?)
{% endhighlight %}

### update

#### Updating data

SQL:

{% highlight sql %}
UPDATE employee
SET lname = 'Bush',
     dept_id = 3
WHERE emp_id = 10;
{% endhighlight %}

HRR:

{% highlight haskell %}
updateEmployee_o3 :: Update ()
updateEmployee_o3 = typedUpdate tableOfEmployee . updateTarget $ \proj -> do
  Employee.lname' <-# value "Bush"
  Employee.deptId' <-# just (value 3)
  wheres $ proj ! Employee.empId' .=. value 10
{% endhighlight %}

Generated SQL:

{% highlight sql %}
UPDATE MAIN.employee
SET lname = 'Bush', dept_id = 3
WHERE (emp_id = 10)
{% endhighlight %}

#### Updating data using correlated subqueries

SQL:

{% highlight sql %}
UPDATE account
SET last_activity_date =
   (SELECT MAX(t.txn_date)
    FROM transaction0 t
    WHERE t.account_id = account.account_id)
WHERE EXISTS (SELECT 1
              FROM transaction0 t
              WHERE t.account_id = account.account_id);
{% endhighlight %}

HRR:

{% highlight haskell %}
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
{% endhighlight %}

Generated SQL:

{% highlight sql %}
UPDATE MAIN.account
SET last_activity_date = 
  date((SELECT ALL MAX (T1.f0) AS f0
        FROM (SELECT ALL T0.txn_date AS f0
             FROM MAIN.transaction0 T0
             WHERE (T0.account_id = account_id)) T1))
WHERE (EXISTS (SELECT ALL 1 AS f0
               FROM MAIN.transaction0 T2
               WHERE (T2.account_id = account_id)))
{% endhighlight %}

### delete

#### Deleting data

SQL:

{% highlight sql %}
DELETE FROM account
WHERE account_id = 2;
{% endhighlight %}

HRR:

{% highlight haskell %}
deleteAccount_o1 :: Delete ()
deleteAccount_o1 = typedDelete tableOfAccount . restriction $ \proj -> do
  wheres $ proj ! Account.accountId' .=. value 2
{% endhighlight %}

Generated SQL:

{% highlight sql %}
DELETE FROM MAIN.account
WHERE (account_id = 2)
{% endhighlight %}

#### Deleting data with conditions

SQL:

{% highlight sql %}
DELETE FROM account
WHERE account_id >= 10 AND account_id <= 20;
{% endhighlight %}

HRR:

{% highlight haskell %}
deleteAccount_o2 :: Delete ()
deleteAccount_o2 = typedDelete tableOfAccount . restriction $ \proj -> do
  wheres $ proj ! Account.accountId' .>=. value 10
  wheres $ proj ! Account.accountId' .<=. value 20
{% endhighlight %}

Generated SQL:

{% highlight sql %}
DELETE FROM MAIN.account
WHERE ((account_id >= 10) AND (account_id <= 20))
{% endhighlight %}

#### Deleting data using correlated subqueries

SQL:

{% highlight sql %}
DELETE FROM department d
WHERE NOT EXISTS (SELECT 1
                  FROM employee e
                  WHERE e.dept_id = d.dept_id);
{% endhighlight %}

HRR:

{% highlight haskell %}
deleteEmployee_9_4_2 :: Delete ()
deleteEmployee_9_4_2 = typedDelete tableOfDepartment . restriction $ \proj -> do
  el <- queryList $ relation $ do
    e <- query employee
    wheres $ e ! Employee.deptId' .=. just (proj ! Department.deptId')
    return (value (1 :: Int64))
  wheres $ not' . exists $ el
{% endhighlight %}

Generated SQL:

{% highlight sql %}
DELETE FROM MAIN.department
WHERE (NOT (EXISTS (SELECT ALL 1 AS f0
                   FROM MAIN.employee T0
                   WHERE (T0.dept_id = dept_id))))
{% endhighlight %}

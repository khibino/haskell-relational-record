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

HRR: TBD

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

#### Membership conditions using subqueries

SQL:

{% highlight sql %}
SELECT account_id, product_cd, cust_id, avail_balance
FROM account
WHERE product_cd IN (SELECT product_cd FROM product
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

### insert

TBD

### update

TBD

### delete

TBD

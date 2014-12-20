---
layout: default
title: Tutorial
---

### Preparing

This is a tiny tutorial of Haskell Relational Record (HRR). This tutorial assumes that SQLite version 3 and HRR are already installed. If not, please install them first (see [quick start](http://khibino.github.io/haskell-relational-record/quickstart.html)).

Also, please download "relational-record-examples" as follows:

    % cabal unpack relational-record-examples
    % cd relational-record-examples-<VERSION>

### Creating tables in a DB

We use the bank example in [Learning SQL](http://shop.oreilly.com/product/9780596007270.do). Its support page provides a script to create the tables of the bank examples for MySQL. We modified [it for SQLite](https://github.com/khibino/haskell-relational-record/blob/master/relational-record-examples/sql/add.sql) and created a DB file called "examples.db" in the top directory of "relational-record-examples". We deeply thank Alan Beaulieu, the author of "Learning SQL".

Note that HRR does not have a feature to create tables at this moment. This is another reason why we provide the DB file.

### Defining record types in Haskell

Now we map the type of rows of a table to a Haskell record type. Here is the schema of the Account table:

    % sqlite3 examples.db
    sqlite> .schema Account
    CREATE TABLE account
     (account_id integer primary key autoincrement not null,
      product_cd varchar(10) not null,
      cust_id integer not null,
      open_date date not null,
      close_date date,
      last_activity_date date,
      status text not null,
      open_branch_id integer,
      open_emp_id integer,
      avail_balance float(10,2),
      pending_balance float(10,2),
      check(status = 'ACTIVE' or status = 'CLOSED' or status = 'FROZEN')
      constraint fk_product_cd foreign key (product_cd)
        references product (product_cd),
      constraint fk_a_cust_id foreign key (cust_id)
        references customer (cust_id),
      constraint fk_a_branch_id foreign key (open_branch_id)
        references branch (branch_id),
      constraint fk_a_emp_id foreign key (open_emp_id)
            references employee (emp_id)
     );

We don't want to define `data Account` for this by hand. HRR gains access to our DB at compile time and automatically generates Haskell record types. To avoid the conflict of record field names, we recommend to make one module per table. (This limitation would be solved by OverloadedFieldRecord in the future.)

Here is the content of "Account.hs":

{% highlight haskell %}
{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Account where

import DataSource (defineTable)

$(defineTable "account")
{% endhighlight %}

This code generates `data Account` and definitions of this DSL like bellows.

{% highlight haskell %}
data Account
  = Account {accountId :: !GHC.Int.Int64,
             productCd :: !String,
             custId :: !GHC.Int.Int64,
             openDate :: !Day,
             closeDate :: !(Maybe Day),
             lastActivityDate :: !(Maybe Day),
             status :: !String,
             openBranchId :: !(Maybe GHC.Int.Int64),
             openEmpId :: !(Maybe GHC.Int.Int64),
             availBalance :: !(Maybe Double),
             pendingBalance :: !(Maybe Double)}
  deriving (Show)

-- Relation type corresponding to Table
account :: Relation () Account
account =  ...

-- Column selectors for This DSL
accountId' :: Pi Account GHC.Int.Int64
accountId'
  = definePi 0
productCd' :: Pi Account String
productCd'
  = definePi 1
custId' :: Pi Account GHC.Int.Int64
custId'
  = definePi 2
....
{% endhighlight %}

"DataSource.hs" is a bit complicated:

{% highlight haskell %}
{-# LANGUAGE TemplateHaskell #-}

module DataSource (
    connect, convTypes, defineTable
  ) where

import Data.Time (Day, LocalTime)
import Database.HDBC.Query.TH (defineTableFromDB)
import Database.HDBC.Schema.Driver (typeMap)
import Database.HDBC.Schema.SQLite3 (driverSQLite3)
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import Database.Record.TH (derivingShow)
import Language.Haskell.TH (Q, Dec, TypeQ)
import Language.Haskell.TH.Name.CamelCase (ConName)

connect :: IO Connection
connect = connectSqlite3 "examples.db"

convTypes :: [(String, TypeQ)]
convTypes =
    [ ("float", [t|Double|])
    , ("date", [t|Day|])
    , ("datetime", [t|LocalTime|])
    , ("double", [t|Double|])
    , ("varchar", [t|String|])
    ]

defineTable :: String -> Q [Dec]
defineTable tableName =
  defineTableFromDB
    connect
    (driverSQLite3 { typeMap = convTypes }) -- overwrite the default type map with yours
    "main" -- schema name, ignored by SQLite
    tableName
    [derivingShow]
{% endhighlight %}

* `Connection` to "examples.db" will be made.
* `convTypes` defines data mappings for ambiguous types in SQLite. You don't have to understand this at this moment.
* `defineTable` is a wrapper for the magic function `defineTableFromDB` which is the heart of code generation.

Using 'DataSource.hs', we need to prepare modules for other tables than Account, of course.

### Defining relations

Next we define a simple relation in "src/examples.hs":

{% highlight haskell %}
account1 :: Relation () Account
account1 = relation $ do
  a <- query account
  wheres $ a ! Account.productCd' `in'` values ["CHK", "SAV", "CD", "MM"]
  return a
{% endhighlight %}

`Relation` takes two type parameters. The first one is the type of placeholder. This example does not use place holder, so its type is (). The second one is type of value in 'Relation'.

Let's see the signature of 'relation':

{% highlight haskell %}
relation :: QuerySimple (Projection Flat r) -> Relation () r
{% endhighlight %}

So, the type of `do` should be `QuerySimple (Projection Flat r)`. `query` has the following type (note that this signature is simplified):

{% highlight haskell %}
query :: Relation () r -> QuerySimple (Projection Flat r)
{% endhighlight %}

`account` is the variable which refers to the "Account" table. This is automatically generated by `defineTableFromDB` and its type is `Relation () r`. So `a <- query account` binds the variable `a` to each row of the "Account" table.

`wheres` is corresponding to the SQL 'where' clause. In this example, rows whose `productCd` is one of "CHK", "SAV", "CD", and "MM" are filtered.

### Connecting to the DB

Let's define a wrapper function to execute our relation on "examples.db":

{% highlight haskell %}
run :: (Show a, IConnection conn, FromSql SqlValue a, ToSql SqlValue p)
       => conn -> p -> Relation p a -> IO ()
run conn param rel = do
  putStrLn $ "SQL: " ++ show rel
  records <- runQuery conn (relationalQuery rel) param
  mapM_ print records
  putStrLn ""
{% endhighlight %}

`run` shows a generated SQL statement first and the results of the query. Here are the signatures of the two important functions above:

{% highlight haskell %}
runQuery :: (IConnection conn, ToSql SqlValue p, FromSql SqlValue a) =>
            conn -> Query p a -> p -> IO [a]

relationalQuery :: Relation p r -> Query p r
{% endhighlight %}

OK. Let's execute our relation on "examples.db":

    % cabal configure
    % cabal build
    % cabal repl executable:examples
    > conn <- connect
    > run conn () account1
    SQL: SELECT ALL T0.account_id AS f0, T0.product_cd AS f1, T0.cust_id AS f2, T0.open_date AS f3, T0.close_date AS f4, T0.last_activity_date AS f5, T0.status AS f6, T0.open_branch_id AS f7, T0.open_emp_id AS f8, T0.avail_balance AS f9, T0.pending_balance AS f10 FROM MAIN.account T0 WHERE (T0.product_cd IN ('CHK', 'SAV', 'CD', 'MM'))
    Account {accountId = 1, productCd = "CHK", custId = 1, openDate = 2000-01-15, closeDate = Nothing, lastActivityDate = Just 2005-01-04, status = "ACTIVE", openBranchId = Just 2, openEmpId = Just 10, availBalance = Just 1057.75, pendingBalance = Just 1057.75}
    Account {accountId = 2, productCd = "SAV", custId = 1, openDate = 2000-01-15, closeDate = Nothing, lastActivityDate = Just 2004-12-19, status = "ACTIVE", openBranchId = Just 2, openEmpId = Just 10, availBalance = Just 500.0, pendingBalance = Just 500.0}
    ...

We make it!

To understand how to express more complicated relations and how to update tables, please read [Examples](examples.html).

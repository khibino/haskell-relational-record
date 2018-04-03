---
layout: default
title: Frequently Asked Questions
---

### Frequently Asked Questions

Q. <a name="runquery-empty-result"></a> I received empty result from ```runQuery```.

A. ```runQuery``` is lazy-IO API.
Trying strict API like ```runQuery'``` instead of lazy-IO API may help to resolve your problem.

Q. <a name="mysql-commands-out-of-sync"></a> I received "Commands out of sync; ..." error of MySQL

A. Lazy-IO API like ```runQuery``` may cause not expected sequence of low-level primitives in RDBMs drivers.
Trying strict API like ```runQuery'``` instead of lazy-IO API may help to resolve your problem.

ex. http://kakkun61.hatenablog.com/entry/2017/02/28/Yesod_%E3%81%A8_HDBC-mysql_%E3%81%A8_haskell-relational-record_%E3%81%A7_%E2%80%9CCommands_out_of_sync%E2%80%9D

Q. <a name="mysql-table-does-not-exist"></a> I received "Table xxx doesn't exist" error of MySQL.

A. Table names of SQL are case sensitive in MySQL default configurations on UNIX systems.
So, you may want to modify MySQL configuration of system variable 'lower_case_table_names = 1'.
( [MySQL case sensitivity](https://dev.mysql.com/doc/refman/en/identifier-case-sensitivity.html) )

Q. <a name="mysql-or-sqlite-3-unions"></a> I received SQL syntax errors of MySQL or SQLite3 for union statement like ```rel1 `unionAll` rel2 `unionAll` rel3```.

A. The only sub-set SQL syntax support of MySQL or SQLite3 causes this problem.
You can avoid this problem like ```relation (query $ rel1 `unionAll` rel2) `unionAll` rel3```.
There is more detail information at [the discussion on this pull-request](https://github.com/khibino/haskell-relational-record/pull/27).

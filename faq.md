---
layout: default
title: Frequently Asked Questions
---

### Frequently Asked Questions

Q. I received "Table xxx doesn't exist" error of MySQL.

A. Table names of SQL are case sensitive in MySQL default configurations on UNIX systems.
So, you may want to modify MySQL configuration of system variable 'lower_case_table_names = 1'.
( https://dev.mysql.com/doc/refman/5.7/en/identifier-case-sensitivity.html )

Q. I received SQL syntax errors of MySQL or SQLite3 for union statement like ```rel1 `unionAll` rel2 `unionAll` rel3```.

A. The only sub-set SQL syntax support of MySQL or SQLite3 causes this problem.
You can avoid this problem like ```relation (query $ rel1 `unionAll` rel2) `unionAll` rel3```.
There is detail information about this issue at https://github.com/khibino/haskell-relational-record/pull/27.

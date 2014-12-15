#! /bin/bash

# Range condition with the between operator

sqlite3 examples.db "
SELECT emp_id, fname, lname, start_date FROM employee
WHERE start_date
BETWEEN date('2001-01-01') AND date('2002-12-31')
;"

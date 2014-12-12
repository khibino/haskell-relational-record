#! /bin/bash

sqlite3 test.db "
SELECT e.fname, e.lname, d.name
FROM employee e INNER JOIN department d
USING (dept_id)
;"

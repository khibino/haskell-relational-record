#! /bin/bash

psql testdb -c "
SELECT e.fname, e.lname, d.name
FROM LEARNINGSQL.employee e INNER JOIN LEARNINGSQL.department d
USING (dept_id)
;"

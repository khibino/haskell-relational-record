#! /bin/bash

psql testdb -c "
SELECT e.fname, e.lname, e_mgr.fname mgr_fname, e_mgr.lname mgr_lname
FROM LEARNINGSQL.employee e INNER JOIN LEARNINGSQL.employee e_mgr
ON e.superior_emp_id = e_mgr.emp_id
;"

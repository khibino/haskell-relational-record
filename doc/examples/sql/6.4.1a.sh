#! /bin/bash

psql testdb -c "
SELECT emp_id, assigned_branch_id
FROM LEARNINGSQL.employee
WHERE title = 'Teller'
UNION
SELECT open_emp_id, open_branch_id
FROM LEARNINGSQL.account
WHERE product_cd = 'SAV'
ORDER BY emp_id
;"

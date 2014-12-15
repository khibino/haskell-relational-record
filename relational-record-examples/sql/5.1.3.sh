#! /bin/bash

# Complex join

sqlite3 examples.db "
SELECT a.account_id, a.cust_id, a.open_date, a.product_cd
FROM account a INNER JOIN employee e ON a.open_emp_id = e.emp_id
INNER JOIN branch b ON e.assigned_branch_id = b.branch_id
WHERE e.start_date <= date('2004-01-01') AND
     (e.title = 'Teller' OR e.title = 'Head Teller') AND
     b.name = 'Woburn Branch'
;"

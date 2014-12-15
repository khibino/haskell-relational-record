#! /bin/bash

# The order by clause

sqlite3 examples.db "
SELECT open_emp_id, product_cd
FROM account
ORDER BY open_emp_id, product_cd
;"

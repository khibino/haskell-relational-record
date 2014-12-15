#! /bin/bash

# Grouping

sqlite3 examples.db "
SELECT open_emp_id, COUNT(*) how_many
FROM account
GROUP BY open_emp_id
ORDER BY open_emp_id
;"

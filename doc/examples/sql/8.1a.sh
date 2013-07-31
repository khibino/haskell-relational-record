#! /bin/bash

psql testdb -c "
SELECT open_emp_id, COUNT(*) how_many
FROM LEARNINGSQL.account
GROUP BY open_emp_id
ORDER BY open_emp_id
;"

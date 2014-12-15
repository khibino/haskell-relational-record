#! /bin/bash

# Sorting via numeric placeholders

sqlite3 examples.db "
SELECT emp_id, title, start_date, fname, lname
FROM employee
ORDER BY 2,5
;"

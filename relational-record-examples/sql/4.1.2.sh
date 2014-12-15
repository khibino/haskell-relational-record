#! /bin/bash

# Using the not operator

sqlite3 examples.db "
SELECT *
FROM employee
WHERE end_date IS NULL AND (title = 'Teller' OR start_date < '2003-01-01')
;"

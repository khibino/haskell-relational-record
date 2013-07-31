#! /bin/bash

psql testdb -c "
SELECT account_id, product_cd, cust_id, avail_balance
FROM LEARNINGSQL.account
WHERE product_cd IN ('CHK', 'SAV', 'CD', 'MM')
;"

#! /bin/bash

# Membership conditions

sqlite3 examples.db "
SELECT account_id, product_cd, cust_id, avail_balance
FROM account
WHERE product_cd IN ('CHK', 'SAV', 'CD', 'MM')
;"

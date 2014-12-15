#! /bin/bash

# Membership conditions using subqueries

sqlite3 examples.db "
SELECT account_id, product_cd, cust_id, avail_balance
FROM account
WHERE product_cd IN (SELECT product_cd FROM product
WHERE product_type_cd = 'ACCOUNT')
;"


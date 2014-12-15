#! /bin/bash

# Descending sort order

sqlite3 examples.db "
SELECT account_id, product_cd, open_date, avail_balance
FROM account
ORDER BY avail_balance DESC
;"

#! /bin/sh

create_keytest_table='
CREATE TABLE EXAMPLE2.keytest (
 name VARCHAR(30) NOT NULL,
 bar INTEGER NOT NULL,
 foo INTEGER NOT NULL,
 attr VARCHAR(30) NOT NULL,
 baz INTEGER NOT NULL,
 
 PRIMARY KEY(foo, name, bar)
)
'

set -x

psql -c "CREATE SCHEMA EXAMPLE2" testdb
psql -c "$create_keytest_table" testdb

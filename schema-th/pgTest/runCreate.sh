#! /bin/sh

PATH='/usr/bin:/bin'

create0='
CREATE TABLE TEST.test_table0 (
 foo INTEGER NOT NULL,
 bar VARCHAR(10),
 baz date,

 PRIMARY KEY(foo)
)
'

set -x

psql -c "CREATE SCHEMA TEST" testdb
psql -c "$create0" testdb


#! /bin/sh

PATH='/usr/bin:/bin'

create0='
CREATE TABLE TEST.test_table0 (
 foo smallint NOT NULL,
 foo_bar integer NOT NULL,
 par_ent integer NOT NULL,
 bar date,
 bar_baz text,
 baz VARCHAR(10),

 PRIMARY KEY(foo_bar)
)
'

set -x

psql -c "CREATE SCHEMA TEST" testdb
psql -c "$create0" testdb


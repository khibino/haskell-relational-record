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

create1='
CREATE TABLE TEST.test_table1 (
  foo integer NOT NULL,

  PRIMARY KEY (foo)
)
'

set -x

psql -c "CREATE SCHEMA TEST" testdb
psql -c "$create0" testdb
psql -c "$create1" testdb

psql -c "INSERT INTO TEST.test_table1 (foo) VALUES (1)" testdb

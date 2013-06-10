#! /bin/sh

PATH='/usr/bin:/bin'
TESTDB=$1

create0='
CREATE TABLE main.test_table0 (
 foo smallint NOT NULL,
 foo_bar integer NOT NULL,
 par_ent integer NOT NULL,
 bar date,
 bar_baz text,
 baz VARCHAR(10),

 PRIMARY KEY(foo_bar)
);
'

create1='
CREATE TABLE main.test_table1 (
  foo integer NOT NULL,

  PRIMARY KEY (foo)
);
'

set -x

sqlite3 "$TESTDB" <<EOS
$create0
$create1
EOS

sqlite3 "$TESTDB" <<EOS
INSERT INTO main.test_table1 (foo) VALUES (1);
INSERT INTO main.test_table1 (foo) VALUES (2);
EOS

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

create2='
CREATE TABLE main.test_table2 (
  x ntext,
  y nvarchar (16) NOT NULL,
  z nchar    (16) NOT NULL
);
'

create3='
CREATE TABLE main.test_table3 (
  name text NOT NULL,
  birth date NOT NULL,
  sex  integer NOT NULL,
  height integer NOT NULL,

  PRIMARY KEY (name, sex, birth)
);
'

set -x

sqlite3 "$TESTDB" <<EOS
$create0
$create1
$create2
$create3
EOS

sqlite3 "$TESTDB" <<EOS
INSERT INTO main.test_table1 (foo) VALUES (1);
INSERT INTO main.test_table1 (foo) VALUES (2);
INSERT INTO main.test_table2 (x, y, z) VALUES ('実験', '実験', '実験');
INSERT INTO main.test_table2 (x, y, z) VALUES ('鷗鄧', '鷗鄧', '鷗鄧');
INSERT INTO main.test_table2 (x, y, z) VALUES ('𦿶丈', '𦿶丈', '𦿶丈');
INSERT INTO main.test_table3 (name, sex, birth, height) VALUES ('Ichiro Yamada', 1, '1955-03-01', 178);
INSERT INTO main.test_table3 (name, sex, birth, height) VALUES ('Hanako Sato', 2, '1973-04-01', 165);
EOS

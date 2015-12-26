#! /bin/sh

create_setA_table='
CREATE TABLE EXAMPLE3.set_a (
 seq  INTEGER NOT NULL,
 name VARCHAR(30) NOT NULL,

 PRIMARY KEY(seq)
)
'

create_setB_table='
CREATE TABLE EXAMPLE3.set_b (
 seq  INTEGER NOT NULL,
 name VARCHAR(30) NOT NULL,

 PRIMARY KEY(seq)
)
'

create_history_table='
CREATE TABLE EXAMPLE3.history (
  seq INTEGER NOT NULL,
  register_time TIMESTAMP NOT NULL,
  log VARCHAR(30) NOT NULL,

  PRIMARY KEY(seq)
)
'

set -x

psql -c "CREATE SCHEMA EXAMPLE3" testdb
psql -c "$create_setA_table" testdb
psql -c "$create_setB_table" testdb
psql -c "$create_history_table" testdb

insert() {
	psql -c "INSERT INTO EXAMPLE3.set_$1 (seq, name) VALUES ($2, '$3')" testdb
}

insert a 1 'Apple'
insert a 2 'Orange'
insert a 5 'Banana'
insert a 6 'Cherry'

insert b 2 'Orange'
insert b 6 'Cherry'
insert b 7 'Melon'

insertHistory() {
	psql -c "INSERT INTO EXAMPLE3.history (seq, register_time, log) VALUES ($1, '$2', '$3')" testdb
}

insertHistory 1 '2013-03-05 17:44:02' 'start'

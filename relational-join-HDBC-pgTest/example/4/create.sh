#! /bin/sh

create_setA_table='
CREATE TABLE EXAMPLE4.set_a (
 seq  INTEGER NOT NULL,
 name VARCHAR(30) NOT NULL,

 PRIMARY KEY(seq)
)
'

create_setB_table='
CREATE TABLE EXAMPLE4.set_b (
 seq  INTEGER NOT NULL,
 name VARCHAR(30) NOT NULL,

 PRIMARY KEY(seq)
)
'

set -x

psql -c "CREATE SCHEMA EXAMPLE4" testdb
psql -c "$create_setA_table" testdb

insert() {
	psql -c "INSERT INTO EXAMPLE4.set_$1 (seq, name) VALUES ($2, '$3')" testdb
}

insert a 1 'Apple'
insert a 2 'Orange'
insert a 5 'Banana'
insert a 6 'Cherry'

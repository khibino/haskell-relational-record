#! /bin/sh

create_one_table='
CREATE TABLE EXAMPLE4.one (
 seq SERIAL NOT NULL,
 data INTEGER NOT NULL,
 PRIMARY KEY (seq)
)
'

create_stock_goods_table='
CREATE TABLE EXAMPLE4.stock_goods (
 seq  INTEGER NOT NULL,
 name VARCHAR(30) NOT NULL,
 unit INTEGER NOT NULL,
 amount INTEGER NOT NULL,

 PRIMARY KEY(seq)
)
'

set -x

psql -c "CREATE SCHEMA EXAMPLE4" testdb
psql -c "$create_one_table" testdb
psql -c "$create_stock_goods_table" testdb

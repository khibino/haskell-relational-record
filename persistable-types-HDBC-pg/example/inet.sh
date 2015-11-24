#!/bin/sh

table=EXAMPLE.inet_example

create() {
    cat <<EOF
CREATE SCHEMA IF NOT EXISTS EXAMPLE;

CREATE TABLE $table
  ( id INTEGER NOT NULL
  , ex_addr inet NOT NULL
  , ex_cidr cidr NOT NULL

  , PRIMARY KEY(id)
  );

\d $table;
EOF
}

set -x

set -e

create | psql testdb

psql -c "
INSERT INTO EXAMPLE.inet_example
VALUES  (0, inet '192.168.0.1/0', cidr '192.168.0')
" testdb

psql -c "
INSERT INTO EXAMPLE.inet_example
VALUES  (1, inet '192.168.0.1/32', cidr '192.168.1')
" testdb

psql -c "
INSERT INTO EXAMPLE.inet_example
VALUES  (2, inet '2001:1::0'  , cidr '2001::1')
" testdb

psql -c "
INSERT INTO EXAMPLE.inet_example
VALUES  (3, inet '2001:2::'  , cidr '2001::1:0')
" testdb

psql -c "SELECT * FROM EXAMPLE.inet_example" testdb

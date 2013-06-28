#! /bin/sh

set -x

psql -c "DROP TABLE EXAMPLE3.set_b" testdb
psql -c "DROP TABLE EXAMPLE3.set_a" testdb
psql -c "DROP SCHEMA IF EXISTS EXAMPLE3" testdb

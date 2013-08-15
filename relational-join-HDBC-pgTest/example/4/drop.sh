#! /bin/sh

set -x

psql -c "DROP TABLE EXAMPLE4.set_a" testdb
psql -c "DROP SCHEMA IF EXISTS EXAMPLE4" testdb

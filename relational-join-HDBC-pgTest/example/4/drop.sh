#! /bin/sh

set -x

psql -c "DROP TABLE EXAMPLE4.stock" testdb
psql -c "DROP SCHEMA IF EXISTS EXAMPLE4" testdb

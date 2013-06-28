#! /bin/sh

set -x

psql -c "DROP TABLE EXAMPLE2.keytest" testdb
psql -c "DROP SCHEMA IF EXISTS EXAMPLE2" testdb

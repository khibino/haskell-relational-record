#! /bin/sh

set -x

psql -c "DROP TABLE EXAMPLE1.membership" testdb
psql -c "DROP TABLE EXAMPLE1.group" testdb
psql -c "DROP TABLE EXAMPLE1.user" testdb
psql -c "DROP SCHEMA IF EXISTS EXAMPLE1" testdb

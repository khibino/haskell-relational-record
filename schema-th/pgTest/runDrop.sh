#! /bin/sh

PATH='/usr/bin:/bin'

set -x

psql -c "DROP TABLE TEST.test_table0" testdb
psql -c "DROP SCHEMA IF EXISTS TEST" testdb

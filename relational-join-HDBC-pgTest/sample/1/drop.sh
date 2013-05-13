#! /bin/sh

PATH='/usr/bin:/bin'

set -x

psql -c "DROP TABLE SAMPLE1.membership" testdb
psql -c "DROP TABLE SAMPLE1.group" testdb
psql -c "DROP TABLE SAMPLE1.user" testdb
psql -c "DROP SCHEMA IF EXISTS SAMPLE1" testdb

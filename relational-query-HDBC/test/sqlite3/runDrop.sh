#! /bin/sh

PATH='/usr/bin:/bin'
TESTDB=$1

set -x

sqlite3 "$TESTDB" <<EOS
DROP TABLE main.test_table1;
DROP TABLE main.test_table0;
EOS

#!/bin/sh

set -x

. ./sh

createdb_command() {
    cat <<EOF
CREATE SCHEMA ARBITRARY0;
GRANT ALL ON ARBITRARY0.* TO ${USER}@'localhost';
GRANT GRANT OPTION ON ARBITRARY0.* TO ${USER}@'localhost';
EOF
}

do_mysql_root createdb_command

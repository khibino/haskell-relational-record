#!/bin/sh

set -x

. ./sh

createuser_command() {
    cat <<EOF
CREATE USER ${USER}@localhost IDENTIFIED BY 'testpassword';
EOF
}

do_mysql_root createuser_command

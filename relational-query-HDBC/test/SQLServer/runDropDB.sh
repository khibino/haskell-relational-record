#! /bin/sh

PATH='/usr/local/bin:/usr/bin:/bin'

usage() {
    echo "Usage: ./runDropDB.sh [-D] -S <server> -U <user> -P <pass>"
    echo "  -S <server>  Server or DSN if -D is provided"
    echo "               examples:"
    echo "                 -S 127.0.0.1"
    echo "                 -S 127.0.0.1¥instanceA"
    echo "                 -S 127.0.0.1,1433"
    echo "                 -D -S testdb"
    echo "  -U <user>    Login ID"
    echo "  -P <pass>    Password"
    exit 1
}

DSN=0
SERVER=
PORT=1433
USER=
PASS=

eval set -- "`getopt DS:P:U:P: $*`"
for opt; do
    case $opt in
        -D )
            DSN=1;;
        -S )
            SERVER=$2;;
        -U )
            USER=$2;;
        -P )
            PASS=$2;;
    esac
    shift
done

if [ -z "$SERVER" -o -z "$USER" -o -z "$PASS" ]; then
    usage
fi

#set -x

STDOUT=
ERROR_STDERR=
GO=
QUIT=

if [ "$DSN" -eq 0 ]; then
    which sqlcmd > /dev/null 2>&1
    if [ "$?" -eq 0 ]; then
        SQLCMD="sqlcmd -S $SERVER -U $USER -P $PASS"
        STDOUT="/dev/null"
        ERROR_STDERR=":error STDERR"
        GO="GO"
    else
        usage
    fi
else
    which sqlcmd > /dev/null 2>&1
    if [ "$?" -eq 0 ]; then
        SQLCMD="sqlcmd -D -S $SERVER -U $USER -P $PASS"
        STDOUT="/dev/null"
        ERROR_STDERR=":error STDERR"
        GO="GO"
    else
        SQLCMD="isql -n $SERVER $USER $PASS"
        STDOUT="/dev/null" # "/dev/fd/1"
        GO=";"
        QUIT="\quit"
    fi
fi

$SQLCMD > $STDOUT <<EOS
$ERROR_STDERR
USE [master]
$GO

IF DB_ID(N'testdb') IS NOT NULL
BEGIN
    DROP DATABASE [testdb]
END
$GO

IF exists (select * from sys.server_principals where name = N'test')
BEGIN
    DROP LOGIN test
END
$GO

$QUIT
EOS

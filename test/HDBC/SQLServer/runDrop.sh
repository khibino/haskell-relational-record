#! /bin/sh

PATH='/usr/local/bin:/usr/bin:/bin'

usage() {
    echo "Usage: ./runDrop.sh [-D] -S <server> -U <user> -P <pass>"
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
USE [testdb]
$GO

IF exists (select * from dbo.sysobjects where id = object_id(N'[TEST].[test_table3]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
BEGIN
    drop table [TEST].[test_table3]
END
$GO

IF exists (select * from dbo.sysobjects where id = object_id(N'[TEST].[test_table2]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
BEGIN
    drop table [TEST].[test_table2]
END
$GO

IF exists (select * from dbo.sysobjects where id = object_id(N'[TEST].[test_table1]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
BEGIN
    drop table [TEST].[test_table1]
END
$GO

IF exists (select * from dbo.sysobjects where id = object_id(N'[TEST].[test_table0]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
BEGIN
    drop table [TEST].[test_table0]
END
$GO

$QUIT
EOS

$SQLCMD > $STDOUT <<EOS
$ERROR_STDERR
USE [testdb]
$GO

IF exists (select * from sys.schemas where name = N'TEST')
BEGIN
    EXEC ('DROP SCHEMA [TEST]')
END
$GO

$QUIT
EOS

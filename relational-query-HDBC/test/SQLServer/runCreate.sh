#! /bin/sh

PATH='/usr/local/bin:/usr/bin:/bin'

usage() {
    echo "Usage: ./runCreate.sh [-D] -S <server> -U <user> -P <pass>"
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

#set -x

create0="
CREATE TABLE [TEST].[test_table0] (
  [foo]      [smallint] NOT NULL ,
  [foo_bar]  [integer]  NOT NULL ,
  [par_ent]  [integer]  NOT NULL ,
  [bar]      [date] ,
  [bar_baz]  [text] ,
  [baz]      [VARCHAR] (10) ,
  CONSTRAINT [pk_test_table0] PRIMARY KEY([foo_bar])
)
"

create1="
CREATE TABLE [TEST].[test_table1] (
  [foo]      [integer]  NOT NULL ,
  CONSTRAINT [pk_test_table1] PRIMARY KEY ([foo])
)
"

create2="
CREATE TABLE [TEST].[test_table2] (
  [x] [ntext] ,
  [y] [nvarchar] (16) NOT NULL ,
  [z] [nchar]    (16) NOT NULL
)
"

create3="
CREATE TABLE [TEST].[test_table3] (
  [name] [nvarchar] (50) NOT NULL,
  [birth] [date] NOT NULL,
  [sex]  [integer] NOT NULL,
  [height] [integer] NOT NULL,

  CONSTRAINT [pk_test_table3] PRIMARY KEY ([name], [sex], [birth])
)
"

$SQLCMD > $STDOUT <<EOS
$ERROR_STDERR
USE [testdb]
$GO

IF not exists (select * from sys.schemas where name = N'TEST')
BEGIN
    EXEC ('CREATE SCHEMA [TEST]')
END
$GO

$QUIT
EOS

$SQLCMD > $STDOUT <<EOS
$ERROR_STDERR
USE [testdb]
$GO

IF exists (select * from dbo.sysobjects where id = object_id(N'[TEST].[test_table0]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
BEGIN
    drop table [TEST].[test_table0]
END
$GO

IF exists (select * from dbo.sysobjects where id = object_id(N'[TEST].[test_table1]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
BEGIN
    drop table [TEST].[test_table1]
END
$GO

IF exists (select * from dbo.sysobjects where id = object_id(N'[TEST].[test_table2]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
BEGIN
    drop table [TEST].[test_table2]
END
$GO

IF exists (select * from dbo.sysobjects where id = object_id(N'[TEST].[test_table3]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
BEGIN
    drop table [TEST].[test_table3]
END
$GO

$create0
$GO

$create1
$GO

$create2
$GO

$create3
$GO

$QUIT
EOS

$SQLCMD > $STDOUT <<EOS
$ERROR_STDERR
USE [testdb]
$GO

INSERT INTO [TEST].[test_table1] ([foo]) VALUES (1)
INSERT INTO [TEST].[test_table1] ([foo]) VALUES (2)
$GO

INSERT INTO [TEST].[test_table2] ([x], [y], [z]) VALUES (N'実験', N'実験', N'実験')
INSERT INTO [TEST].[test_table2] ([x], [y], [z]) VALUES (N'鷗鄧', N'鷗鄧', N'鷗鄧')
INSERT INTO [TEST].[test_table2] ([x], [y], [z]) VALUES (N'𦿶丈', N'𦿶丈', N'𦿶丈')
$GO

INSERT INTO [TEST].[test_table3] ([name], [sex], [birth], [height]) VALUES ('Ichiro Yamada', 1, '1955-03-01', 178)
INSERT INTO [TEST].[test_table3] ([name], [sex], [birth], [height]) VALUES ('Hanako Sato', 2, '1973-04-01', 165)
$GO

$QUIT
EOS

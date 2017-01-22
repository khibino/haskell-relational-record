#!/bin/sh

set -x

createdb hrrtest
echo "CREATE SCHEMA ARBITRARY0" | psql hrrtest

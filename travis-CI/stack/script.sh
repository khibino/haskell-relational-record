#!/bin/sh

set -e

. ./travis-CI/sh-lib
. ./travis-CI/dirs.list

set -x

skip_no_match_branch

STACK_YAML=stack-travis.yaml stack build
STACK_YAML=stack-travis.yaml stack test

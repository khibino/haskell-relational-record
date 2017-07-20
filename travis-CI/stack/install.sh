#!/bin/sh

set -e

. ./travis-CI/sh-lib
. ./travis-CI/dirs.list

set -x

skip_no_match_branch

checkout_root=$(pwd)

(
    show_stack_pkgs
    sed "s/^resolver: .*/resolver: ${STACK_RESOLVER}/" \
        < $checkout_root/travis-CI/stack/template.yaml \
) > stack-travis.yaml

cat stack-travis.yaml

STACK_YAML=stack-travis.yaml stack setup
STACK_YAML=stack-travis.yaml stack install --only-dependencies

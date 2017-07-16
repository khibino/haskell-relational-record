#!/bin/sh

set -e

. ./travis-CI/sh-lib
. ./travis-CI/dirs.list

set -x

skip_no_match_branch

checkout_root=$(pwd)

install_package() {
    sed "s/^resolver: .*/resolver: ${STACK_RESOLVER}/" \
        < $checkout_root/travis-CI/stack/template.yaml \
        > stack-travis.yaml
    ##    stack.yaml must be located the same directory which has *.cabal -- constraint of stack?

    STACK_YAML=stack-travis.yaml stack setup
    STACK_YAML=stack-travis.yaml stack install --only-dependencies
}

if [ x"$dirs" = x ]; then
    install_package
else
    for d in $dirs; do
        ( cd $d && install_package )
    done
fi

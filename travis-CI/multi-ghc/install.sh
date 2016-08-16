#!/bin/sh

do_install() {
    cabal install $CABAL_CONSTRAINTS --only-dependencies --enable-tests --enable-benchmarks
}

. ./travis-CI/multi-ghc/sh-defs

set -x

subdir=relational-record
check_dir $subdir
echo "Prereqs of $name ... "
( cd $subdir && do_install )

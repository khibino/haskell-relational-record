#!/bin/sh

do_install() {
    cabal install --only-dependencies --enable-tests --enable-benchmarks
}

. ./travis-CI/multi-ghc/sh-defs

set -x

for subdir in $subdirs ; do
    check_dir $subdir
    echo "Prereqs of $name ... "
    ( cd $subdir && do_install )
done

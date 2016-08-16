#!/bin/sh

do_script() {
    cabal configure $CABAL_CONSTRAINTS --enable-tests --enable-benchmarks -v2  # -v2 provides useful information for debugging
    cabal build   # this builds all libraries and executables (including tests/benchmarks)
    cabal test
    cabal check
    cabal sdist   # tests that a source-distribution can be generated

    # Check that the resulting source distribution can be built & installed.
    # If there are no other `.tar.gz` files in `dist`, this can be even simpler:
    # `cabal install --force-reinstalls dist/*-*.tar.gz`
    SRC_TGZ=$(cabal info . | awk '{print $2;exit}').tar.gz &&
        (cd dist && cabal install $CABAL_CONSTRAINTS --force-reinstalls "$SRC_TGZ")
}

. ./travis-CI/multi-ghc/sh-defs

set -x

for subdir in $subdirs ; do
    check_dir $subdir
    echo "Install $name ... "
    ( cd $subdir && do_script )
done

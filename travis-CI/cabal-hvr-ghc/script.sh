#!/bin/sh

set -e

. ./travis-CI/dirs.list

set -x

script_build() {
    if [ -f configure.ac ]; then autoreconf -i; fi
    cabal configure $CABAL_FLAGS $CABAL_CONSTRAINTS --enable-tests --enable-benchmarks -v2  # -v2 provides useful information for debugging
    cabal build $CABAL_JOBS  # this builds all libraries and executables (including tests/benchmarks)
    cabal test $CABAL_JOBS
    cabal check
    cabal sdist   # tests that a source-distribution can be generated

    # Check that the resulting source distribution can be built & installed.
    # If there are no other `.tar.gz` files in `dist`, this can be even simpler:
    # `cabal install --force-reinstalls dist/*-*.tar.gz`
    SRC_TGZ=$(cabal info . | awk '{print $2;exit}').tar.gz && \
        (cd dist && cabal install $CABAL_JOBS --force-reinstalls "$SRC_TGZ")
}

if [ x"$dirs" = x ]; then
    script_build
else
    for d in $dirs; do
        ( cd $d && script_build )
    done
fi

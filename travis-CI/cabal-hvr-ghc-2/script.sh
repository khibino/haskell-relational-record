#!/bin/sh

set -e

. ./travis-CI/dirs.list

set -x

script_build() {
    PKGNAME="$1"

    if [ -f configure.ac ]; then autoreconf -i; fi
    rm -rf .ghc.environment.* dist/
    cabal sdist # test that a source-distribution can be generated
    cd dist/
    SRCTAR=$(echo ${PKGNAME}-*.tar.gz)
    SRC_BASENAME="${SRCTAR%.tar.gz}"
    tar -xvf "./${SRC_BASENAME}.tar.gz"
    cd "$SRC_BASENAME/"
    ## from here on, CWD is inside the extracted source-tarball
    rm -fv cabal.project.local
    echo 'packages: .' > cabal.project
    # this builds all libraries and executables (without tests/benchmarks)
    rm -f cabal.project.freeze
    cabal new-build -w ${HC} --disable-tests --disable-benchmarks all
    # this builds all libraries and executables (including tests/benchmarks)
    # - rm -rf ./dist-newstyle

    # build & run tests
    cabal new-build -w ${HC} ${TEST} ${BENCH} all
    if [ "x$TEST" = "x--enable-tests" ]; then cabal new-test -w ${HC} ${TEST} all; fi
}

if [ x"$dirs" = x ]; then
    script_build $PKGNAME
else
    for d in $dirs; do
        ( cd $d && script_build "$d" )
    done
fi

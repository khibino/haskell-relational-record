#!/bin/bash

set -e

. ./travis-CI/dirs.list

set -x

BENCH=${BENCH---enable-benchmarks}
TEST=${TEST---enable-tests}
HADDOCK=${HADDOCK-true}
INSTALLED=${INSTALLED-true}

## ghc-pkg path. ## substitution of bash
HCPKG=${HC/ghc/ghc-pkg}

script_build() {
    PKGNAME="$1"

    DISTDIR=$(mktemp -d /tmp/dist-test.XXXX)
    cabal sdist # test that a source-distribution can be generated
    mv "."/dist/${PKGNAME}-*.tar.gz ${DISTDIR}/
    cd ${DISTDIR} || false
    find . -maxdepth 1 -name '*.tar.gz' -exec tar -xvf '{}' \;

    printf "packages: ${PKGNAME}-"*/*'.cabal\n' > cabal.project
    cat cabal.project

    # this builds all libraries and executables (without tests/benchmarks)
    cabal new-build -w ${HC} --disable-tests --disable-benchmarks all

    # Build with installed constraints for packages in global-db
    if $INSTALLED; then
        echo cabal new-build -w ${HC} --disable-tests --disable-benchmarks $(${HCPKG} list --global --simple-output --names-only | sed 's/\([a-zA-Z0-9-]\{1,\}\) */--constraint="\1 installed" /g') all | sh;
    else
        echo "Not building with installed constraints";
    fi

    # build & run tests, build benchmarks
    cabal new-build -w ${HC} ${TEST} ${BENCH} all
    for p in $test_pkgs ; do
        if [ x$PKGNAME = x$p ]; then
            cabal new-test -w ${HC} ${TEST} ${BENCH} all
            break
        fi
    done

    # cabal check
    (cd ${PKGNAME}-* && cabal check)
    rm -rf ./dist-newstyle

    # haddock
    if $HADDOCK; then cabal new-haddock -w ${HC} ${TEST} ${BENCH} all; else echo "Skipping haddock generation";fi
}

if [ x"$dirs" = x ]; then
    script_build $PKGNAME
else
    for d in $dirs; do
        ( cd $d && script_build "$d" )
    done
fi

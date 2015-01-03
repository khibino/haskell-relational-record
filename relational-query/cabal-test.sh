#! /bin/sh

set -x

cabal clean
cabal configure --enable-tests
cabal build
cabal test

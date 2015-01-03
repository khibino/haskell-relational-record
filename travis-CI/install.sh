#!/bin/sh

set -x

cabal install cabal-test-compat
cd relational-query
cabal install --only-dependencies

#!/bin/sh

set -x

sudo apt-get install haskell-platform haskell-platform-doc haskell-platform-prof
sudo apt-get install cabal-install
sudo apt-get install libghc-dlist-dev libghc-dlist-prof libghc-dlist-doc
sudo apt-get install libghc-convertible-dev libghc-convertible-prof libghc-convertible-doc
sudo apt-get install libghc-hdbc-dev libghc-hdbc-prof libghc-hdbc-doc

cabal install time-locale-compat
cabal install cabal-test-compat

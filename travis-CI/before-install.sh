#!/bin/sh

set -x

##cat /proc/cpuinfo

sudo apt-get update
sudo apt-get install cabal-install
cabal --version
cabal update
sudo apt-get install haskell-platform haskell-platform-prof haskell-platform-doc

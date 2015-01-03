#!/bin/sh

set -x

cat /proc/cpuinfo

sudo apt-get update
sudo apt-get install cabal-install
cabal update

#!/bin/sh

set -x

cat /proc/cpuinfo
sudo apt-get update
cabal update

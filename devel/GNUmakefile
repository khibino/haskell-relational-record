
all: clean build

haddock_opt = --hyperlink-source --haddock-options=--css=../devel/black.css

ncpu = $(shell cat /proc/cpuinfo | egrep '^processor' | wc -l)
jobs = --jobs=$(shell expr $(ncpu) '*' 3 '/' 4)

build:
	cabal v1-configure -O0 --enable-tests
	cabal v1-build $(jobs) --ghc-option=-Wall
	cabal v1-haddock $(haddock_opt)
	cabal v1-test

haddock:
	cabal v1-configure
	cabal v1-haddock $(haddock_opt)

check:
	cabal v1-check

wc:
	make clean-hs
	test ! -d src || find src -name '*.hs' | xargs wc

clean:
	make clean-hs
	cabal v1-clean
	[ ! -d .debian-build ] || rm -r .debian-build
	[ ! -d .stack-work ] || rm -r .stack-work
	test ! -d src || find src \( -name '*.o' -o -name '*.hi' -o -name '*.dyn_o' -o  -name '*.dyn_hi' \) -exec rm {} \;

clean-hs:
	test ! -d src || find src -name '*_flymake.hs' -exec rm {} \;

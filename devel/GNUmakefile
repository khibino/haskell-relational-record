
all: clean build

haddock_opt = --hyperlink-source --haddock-options=--css=../devel/black.css

ncpu = $(shell cat /proc/cpuinfo | egrep '^processor' | wc -l)
jobs = --jobs=$(shell expr $(ncpu) '*' 3 '/' 4)

build:
	cabal configure -O0
	cabal build $(jobs) --ghc-option=-Wall
	cabal haddock $(haddock_opt)
	cabal test

haddock:
	cabal configure
	cabal haddock $(haddock_opt)

check:
	cabal check

wc:
	make clean-hs
	test ! -d src || find src -name '*.hs' | xargs wc

clean:
	make clean-hs
	cabal clean
	[ ! -d .debian-build ] || rm -r .debian-build
	[ ! -d .stack-work ] || rm -r .stack-work
	test ! -d src || find src \( -name '*.o' -o -name '*.hi' -o -name '*.dyn_o' -o  -name '*.dyn_hi' \) -exec rm {} \;

clean-hs:
	test ! -d src || find src -name '*_flymake.hs' -exec rm {} \;


all: clean build

build:
	cabal configure
	cabal build --ghc-option=-Wall
	cabal haddock --hyperlink-source

check:
	cabal check

wc:
	test ! -d src || find src -name '*.hs' | xargs wc

clean:
	cabal clean
	[ ! -d .dWork ] || rm -r .dWork
	test ! -d src || find src \( -name '*.o' -o -name '*.hi' \) -exec rm {} \;


all: clean build

build:
	cabal configure
	cabal build
	cabal haddock --hyperlink-source

wc:
	test ! -d src || find src -name '*.hs' | xargs wc

clean:
	cabal clean
	[ ! -d .dWork ] || rm -r .dWork
	test ! -d src || find src \( -name '*.o' -o -name '*.hi' \) -exec rm {} \;

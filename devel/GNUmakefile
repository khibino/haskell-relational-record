
all: clean build

haddock_opt = --hyperlink-source --haddock-options=--css=../devel/black.css

build:
	cabal configure
	cabal build --ghc-option=-Wall
	cabal haddock $(haddock_opt)

haddock:
	cabal configure
	cabal haddock $(haddock_opt)

check:
	cabal check

wc:
	test ! -d src || find src -name '*.hs' | xargs wc

clean:
	cabal clean
	[ ! -d .dWork ] || rm -r .dWork
	test ! -d src || find src \( -name '*.o' -o -name '*.hi' \) -exec rm {} \;

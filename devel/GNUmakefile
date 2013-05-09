
all: clean
	cabal configure
	cabal build
	cabal haddock --hyperlink-source

wc:
	find src -name '*.hs' | xargs wc

clean:
	cabal clean
	[ ! -d .dWork ] || rm -r .dWork
	find src \( -name '*.o' -o -name '*.hi' \) -exec rm {} \;

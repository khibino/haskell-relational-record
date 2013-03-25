
all: clean
	cabal configure
	cabal build
	cabal haddock --hyperlink-source

clean:
	cabal clean
	[ ! -d .dWork ] || rm -r .dWork
	find src \( -name '*.o' -o -name '*.hi' \) -exec rm {} \;

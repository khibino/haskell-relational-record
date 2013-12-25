
lib_list = \
	names-th \
	sql-words \
	persistable-record \
	relational-query \
	HDBC-session \
	relational-query-HDBC

all:

install:
	for d in $(lib_list) ; do \
		( cd $$d && cabal install ) ; \
	done

clean:
	for d in $(lib_list) ; do \
		( cd $$d && cabal clean ) ; \
	done

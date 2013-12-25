for %%d IN (names-th sql-words persistable-record relational-query session relational-query-HDBC) do (
    cd %%d
    cabal configure
    cabal haddock
    cd ..
)

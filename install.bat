for %%d IN (names-th sql-words DB-record relational-join session relational-query-HDBC) do (
    cd %%d
    cabal install
    cd ..
)

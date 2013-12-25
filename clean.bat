for %%d IN (names-th sql-words persistable-record relational-query HDBC-session relational-query-HDBC) do (
    cd %%d
    cabal clean
    cd ..
)

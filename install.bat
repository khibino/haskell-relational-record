for %%d IN (names-th sql-words persistable-record relational-query relational-schemas HDBC-session relational-query-HDBC) do (
    cd %%d
    cabal install
    cd ..
)

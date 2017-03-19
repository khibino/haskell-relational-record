{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module Transaction where

import Database.Relational.Query (Relation)
import Database.Record.TH.SQLite3 (defineTable)

$(defineTable "examples.db" "transaction0")

type Transaction = Transaction0

transaction :: Relation () Transaction
transaction = transaction0

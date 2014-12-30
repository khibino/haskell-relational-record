{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Transaction where

import Database.Relational.Query (Relation)
import DataSource (defineTable)

$(defineTable "transaction0")

type Transaction = Transaction0

transaction :: Relation () Transaction
transaction = transaction0

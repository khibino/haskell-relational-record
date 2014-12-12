{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Transaction where

import DataSource (defineTable)

$(defineTable "transaction0")

type Transaction = Transaction0

transaction = transaction0

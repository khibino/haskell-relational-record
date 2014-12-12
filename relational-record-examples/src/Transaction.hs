{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Transaction where

import DataSource (defineTable)
import Prelude hiding (id)

$(defineTable "transaction0")

type Transaction = Transaction0

transaction = transaction0

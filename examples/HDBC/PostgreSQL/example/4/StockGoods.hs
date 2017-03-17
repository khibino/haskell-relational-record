{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module StockGoods where

import GHC.Generics (Generic)
import Prelude hiding (seq)
import PgTestDataSource (defineTable)

$(defineTable []
  "EXAMPLE4" "stock_goods" [''Show, ''Generic])

{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module StockGoods where

import Prelude hiding (seq)
import PgTestDataSource (defineTable)

$(defineTable []
  "EXAMPLE4" "stock_goods" [''Show])

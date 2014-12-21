{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Mixed where

import DataSource (defineTable)

$(defineTable "person")
$(defineTable "product")

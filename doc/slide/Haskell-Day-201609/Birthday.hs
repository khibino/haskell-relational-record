{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Birthday where

import DataSource (definePgConTable)

$(definePgConTable "EXAMPLE" "birthday"
  [''Eq, ''Show])

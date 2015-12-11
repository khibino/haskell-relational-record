{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Birthday where

import DS (definePgConTable)

$(definePgConTable "EXAMPLE" "birthday"
  [''Eq, ''Show])

module Tool (
  eq,
  eqShow,
  ) where

import Distribution.TestSuite (Test)
import Distribution.TestSuite.Compat (prop')

eq :: Eq b => String -> (a -> b) -> a -> b -> String -> Test
eq name t x est em = prop' name (Just em) (t x == est)

eqShow :: Show a => String -> a -> String -> Test
eqShow name x est =
  eq name show x est $ unwords [show x, "/=", est]

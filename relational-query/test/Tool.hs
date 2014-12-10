module Tool (
  prop,
  eq,
  eqShow,
  ) where

-- import Control.Exception (try)

import Distribution.TestSuite
  (Test (Test), TestInstance (TestInstance),
   Result (Pass, Fail), Progress (Finished))


noOption :: String -> IO Progress -> Test
noOption name p = Test this  where
  this = TestInstance p name [] [] (\_ _ -> Right this)

simple :: String -> IO Result -> Test
simple name = noOption name . fmap Finished


boolResult :: Bool -> String -> Result
boolResult b msg
  | b          =  Pass
  | otherwise  =  Fail msg

prop :: String -> Bool -> String -> Test
prop name b = simple name . return . boolResult b

eq :: Eq b => String -> (a -> b) -> a -> b -> String -> Test
eq name t x est = prop name (t x == est)

eqShow :: Show a => String -> a -> String -> Test
eqShow name x est =
  eq name show x est $ unwords [show x, "/=", est]

{-
boolIoResult :: IO Bool -> String -> IO Result
boolIoResult iob msg = do
  eb <- try iob
  return $ case eb :: Either IOError Bool of
    Right True   ->  Pass
    Right False  ->  Fail msg
    Left e       ->  Error $ show e

propIO :: String -> IO Bool -> String -> Test
propIO name iob = simple name . boolIoResult iob
 -}

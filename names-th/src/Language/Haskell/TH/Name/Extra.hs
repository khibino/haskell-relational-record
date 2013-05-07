
module Language.Haskell.TH.Name.Extra (
  pprQ,

  compileErrorIO, compileError,

  integralE, simpleValD
  ) where

import Language.Haskell.TH
  (Ppr, ppr, Q, runQ, runIO,
   Name, Dec, sigD, valD, TypeQ, varP, normalB,
   ExpQ, litE, integerL)
import Language.Haskell.TH.PprLib (Doc)
import Language.Haskell.TH.Syntax (Quasi)


pprQ :: (Functor m, Quasi m, Ppr a) => Q a -> m Doc
pprQ =  fmap ppr . runQ

compileErrorIO :: String -> IO a
compileErrorIO =  ioError . userError

compileError :: String -> Q a
compileError =  runIO . compileErrorIO

integralE :: Integral a => a -> ExpQ
integralE =  litE . integerL . toInteger

simpleValD :: Name -> TypeQ -> ExpQ -> Q [Dec]
simpleValD var typ expr =  do
  sig <- sigD var typ
  val <- valD (varP var) (normalB expr) []
  return [sig, val]

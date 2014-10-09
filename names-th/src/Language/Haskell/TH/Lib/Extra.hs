-- |
-- Module      : Language.Haskell.TH.Lib.Extra
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides extra helper functions
-- complementing "Language.Haskell.TH.Lib"
module Language.Haskell.TH.Lib.Extra (
  -- * Extra template functions
  -- $extraTemplateFunctions
  integralE, simpleValD, maybeD,

  -- * Pretty printing for 'Q' monad
  -- $prettyPrint
  pprQ,

  -- * Functions to raise compile error
  -- $compileError
  compileErrorIO, compileError,

  ) where

import Language.Haskell.TH
  (Ppr, ppr, Q, runQ,
   Name, Dec, sigD, valD, TypeQ, varP, normalB,
   ExpQ, litE, integerL)
import Language.Haskell.TH.PprLib (Doc)
import Language.Haskell.TH.Syntax (Quasi)

{- $extraTemplateFunctions
Extra functions to generate haskell templates.
-}

-- | Integer literal template from 'Integral' types.
integralE :: Integral a => a -> ExpQ
integralE =  litE . integerL . toInteger

-- | Generate declaration template from name, type and expression.
simpleValD :: Name -> TypeQ -> ExpQ -> Q [Dec]
simpleValD var typ expr =  do
  sig <- sigD var typ
  val <- valD (varP var) (normalB expr) []
  return [sig, val]

-- | May generate declaration template.
maybeD :: (a -> Q [Dec]) -> Maybe a -> Q [Dec]
maybeD =  maybe (return [])

{- $prettyPrint
Pretty printing for haskell templates.
-}

-- | Helper function for pretty printing 'Q' Monad.
pprQ :: (Functor m, Quasi m, Ppr a) => Q a -> m Doc
pprQ =  fmap ppr . runQ

{- $compileError
Functions to raise compile error from codes
which generating haskell templates.
-}

-- | Raise compile error from TH code.
compileError :: Quasi m => String -> m a
compileError =  runQ . fail

-- | 'IO' version of 'compileError'.
compileErrorIO :: String -> IO a
compileErrorIO =  compileError
{-# DEPRECATED compileErrorIO "Use compileError instead of this" #-}

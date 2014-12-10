-- |
-- Module      : Language.Haskell.TH.Name.CamelCase
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides camelcased 'Name' for Template Haskell
module Language.Haskell.TH.Name.CamelCase (
  -- * Types to wrap 'Name'
  -- $nameTypes
  ConName (ConName, conName), toConName,
  VarName (VarName, varName), toVarName,

  -- * Functions to make camel-cased names
  -- $makeNames
  conCamelcaseName, varCamelcaseName,

  varNameWithPrefix,

  -- * Functions to generate haskell template from names
  -- $makeTemplates
  toTypeCon, toDataCon,

  toVarExp, toVarPat
  ) where

import Data.Char (toUpper, toLower)
import Language.Haskell.TH
  (Name, mkName, TypeQ, conT, ExpQ, conE, varE, PatQ, varP)

capitalize :: String -> String
capitalize (c:cs) = toUpper c : cs
capitalize ""     = ""

unCapitalize :: String -> String
unCapitalize (c:cs) = toLower c : cs
unCapitalize ""     = ""

{- $nameTypes
Wrap 'Name' to distinguish constructor names and variable names.
-}

-- | Type to wrap constructor\'s 'Name'.
newtype ConName = ConName { conName :: Name {- ^ Get wrapped 'Name' -} }

-- | Make constructor name from 'String'.
toConName :: String -> ConName
toConName =  ConName . mkName . capitalize

-- | Type to wrap variable\'s 'Name'.
newtype VarName = VarName { varName :: Name {- ^ Get wrapped 'Name' -} }

-- | Make variable name from 'String'.
toVarName :: String -> VarName
toVarName =  VarName . mkName . unCapitalize

-- | 'Char' set used from camel-cased names.
nameChars :: String
nameChars =  '\'' : ['0' .. '9'] ++ ['A' .. 'Z'] ++  ['a' .. 'z']

-- | Split into chunks to generate camel-cased 'String'.
splitForName :: String -> [String]
splitForName str
  | rest /= [] = tk : splitForName (tail rest)
  | otherwise  = [tk]
  where
    (tk, rest) = span (`elem` nameChars) str

{- $makeNames
Make camel-cased names.
-}

-- | Convert into camel-cased 'String'.
--   First 'Char' of result is upper case.
camelcaseUpper :: String -> String
camelcaseUpper =  concatMap capitalize . splitForName

-- | Make camel-cased constructor name from 'String'.
conCamelcaseName :: String -> ConName
conCamelcaseName =  toConName . camelcaseUpper

-- | Make camel-cased variable name from 'String'.
varCamelcaseName :: String -> VarName
varCamelcaseName =  toVarName . camelcaseUpper

-- | Make camel-cased variable name with prefix like below.
--
-- >  name `varNamePrefix` prefix
--
varNameWithPrefix :: String -> String -> VarName
varNameWithPrefix n p =  toVarName $ p ++ camelcaseUpper n

{- $makeTemplates
Make haskell templates from names.
-}

-- | Make type constructor 'TypeQ' monad from constructor name type.
toTypeCon :: ConName -> TypeQ
toTypeCon =  conT . conName

-- | Make data constructor 'ExpQ' monad from constructor name type.
toDataCon :: ConName -> ExpQ
toDataCon =  conE . conName

-- | Make variable 'ExpQ' monad from variable name type.
toVarExp :: VarName -> ExpQ
toVarExp =  varE . varName

-- | Make pattern 'PatQ' monad from variable name type.
toVarPat :: VarName -> PatQ
toVarPat =  varP . varName

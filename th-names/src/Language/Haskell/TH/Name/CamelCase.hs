
module Language.Haskell.TH.Name.CamelCase (
  ConName (ConName, conName), toConName,
  VarName (VarName, varName), toVarName,

  conCamelcaseName, varCamelcaseName,

  varNameWithPrefix,

  toTypeCon, toDataCon,
  ) where

import Data.Char (toUpper, toLower)
import Language.Haskell.TH
  (Name, mkName, TypeQ, conT, ExpQ, conE)

capitalize :: String -> String
capitalize (c:cs) = toUpper c : cs
capitalize ""     = ""

unCapitalize :: String -> String
unCapitalize (c:cs) = toLower c : cs
unCapitalize ""     = ""

newtype ConName = ConName { conName :: Name }
newtype VarName = VarName { varName :: Name }

toConName :: String -> ConName
toConName =  ConName . mkName . capitalize

toVarName :: String -> VarName
toVarName =  VarName . mkName . unCapitalize

nameChars :: String
nameChars =  '\'' : ['0' .. '9'] ++ ['A' .. 'Z'] ++  ['a' .. 'z']

splitForName :: String -> [String]
splitForName str
  | rest /= [] = tk : splitForName (tail rest)
  | otherwise  = [tk]
  where
    (tk, rest) = span (`elem` nameChars) str

camelcaseUpper :: String -> String
camelcaseUpper =  concat . map capitalize . splitForName

conCamelcaseName :: String -> ConName
conCamelcaseName =  toConName . camelcaseUpper

varCamelcaseName :: String -> VarName
varCamelcaseName =  toVarName . camelcaseUpper

varNameWithPrefix :: String -> String -> VarName
varNameWithPrefix n p =  toVarName $ p ++ camelcaseUpper n

toTypeCon :: ConName -> TypeQ
toTypeCon =  conT . conName

toDataCon :: ConName -> ExpQ
toDataCon =  conE . conName

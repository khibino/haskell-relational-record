
module Database.Relational.Query.AliasId.Unsafe (
  AliasId (AliasId), primAlias
  ) where

newtype AliasId = AliasId Int deriving (Show, Eq)

primAlias :: AliasId
primAlias =  AliasId 0

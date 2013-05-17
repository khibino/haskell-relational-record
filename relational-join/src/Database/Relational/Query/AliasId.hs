
module Database.Relational.Query.AliasId (
  AliasId, newAliasId,

  columnN, aliasName, (<.>), columnFromId,
  asColumnN,

  Qualified, unQualify, qualifyAlias, qualify,

  qualifiedSQLas
  ) where

import qualified Language.SQL.Keyword as SQL
import qualified Language.SQL.Keyword.ConcatString as SQLs
import Database.Relational.Query.AliasId.Unsafe (AliasId(AliasId))

newAliasId :: AliasId -> AliasId
newAliasId (AliasId i) = AliasId $ i + 1

columnN :: Int -> String
columnN i = 'f' : show i

aliasName :: AliasId -> String
aliasName (AliasId i) = 'T' : show i

(<.>) :: String -> String -> String
t <.> n = t ++ '.' : n

columnFromId :: AliasId -> Int -> String
columnFromId a i = aliasName a <.> columnN i

asColumnN :: SQL.Keyword -> Int -> SQL.Keyword
f `asColumnN` n = f `SQL.as` SQL.word (columnN  n)


-- | Qualified relaltion by alias id
data Qualified fr =
  Qualified
  { unQualify    :: fr
  , qualifyAlias :: !AliasId
  }

qualify :: fr -> AliasId -> Qualified fr
qualify =  Qualified

instance Functor Qualified where
  fmap f (Qualified a i) = Qualified (f a) i

qualifiedSQLas :: Qualified String -> String
qualifiedSQLas q =
  unQualify q
  `SQLs.as`
  (aliasName $ qualifyAlias q)

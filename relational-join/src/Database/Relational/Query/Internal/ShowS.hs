module Database.Relational.Query.Internal.ShowS (
  showWordsSQL, showWordSQL, showUnwords
  ) where

import Language.SQL.Keyword (unwordsSQL)
import qualified Language.SQL.Keyword as SQL

showWordsSQL :: [SQL.Keyword] -> ShowS
showWordsSQL =  showString . unwordsSQL

showWordSQL :: SQL.Keyword -> ShowS
showWordSQL =  showString . SQL.wordShow

showUnwords :: [ShowS] -> ShowS
showUnwords =  rec  where
  rec []     = id
  rec [s]    = s
  rec (s:ss@(_:_)) = s . showChar ' ' . rec ss

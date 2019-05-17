-- |
-- Module      : Database.Relational.SqlSyntax.Updates
-- Copyright   : 2013-2018 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides types and expanding operations of SQL update and insert structure.
module Database.Relational.SqlSyntax.Updates (
  -- * Update and Insert assignments
  AssignColumn, AssignTerm, Assignment,

  composeSets,
  composeChunkValues,
  composeChunkValuesWithColumns,
  composeValuesListWithColumns,
  ) where

import Data.Monoid ((<>))

import Language.SQL.Keyword (Keyword(..), (|*|), (.=.))
import qualified Language.SQL.Keyword as SQL
import Database.Relational.SqlSyntax.Types (WithPlaceholderOffsets, SQLWithPlaceholderOffsets')

import Database.Relational.Internal.String (StringSQL, rowConsStringSQL)


-- | Column SQL String of assignment
type AssignColumn = StringSQL

-- | Value SQL String of assignment
type AssignTerm   = StringSQL

-- | Assignment pair
type Assignment = (AssignColumn, AssignTerm)

-- | Compose SET clause from ['Assignment'].
composeSets :: WithPlaceholderOffsets [Assignment] -> SQLWithPlaceholderOffsets'
composeSets as = assigns <$> assignList where
  assignList = foldr (\ (col, term) r ->
                       (col .=. term) : r)
               [] <$> as
  assigns asl | null asl  = error "Update assignment list is null!"
              | otherwise = SET <> SQL.fold (|*|) asl

-- | Compose VALUES clause from a row of value expressions.
composeChunkValues :: Int          -- ^ record count per chunk
                   -> [AssignTerm] -- ^ value expression list
                   -> Keyword
composeChunkValues n0 vs =
    VALUES <> cvs
  where
    n | n0 >= 1    =  n0
      | otherwise  =  error $ "Invalid record count value: " ++ show n0
    cvs = SQL.fold (|*|) . replicate n $ rowConsStringSQL vs

-- | Compose columns row and VALUES clause from a row of value expressions.
composeChunkValuesWithColumns :: Int          -- ^ record count per chunk
                              -> WithPlaceholderOffsets [Assignment] -- ^
                              -> SQLWithPlaceholderOffsets'
composeChunkValuesWithColumns sz asPhs = f <$> asPhs
  where
    f as = rowConsStringSQL cs <> composeChunkValues sz vs
      where
        (cs, vs) = unzip as

-- | Compose columns row and VALUES clause from rows list of value expressions.
composeValuesListWithColumns :: [WithPlaceholderOffsets [Assignment]]
                             -> SQLWithPlaceholderOffsets'
composeValuesListWithColumns pss = f <$> csPhs <*> vssPhs
  where
    f cs vss = rowConsStringSQL cs <> VALUES <> SQL.fold (|*|) (map rowConsStringSQL vss)
    csPhs :: WithPlaceholderOffsets [AssignColumn]
    csPhs = case pss of
           []    ->  error "insertValueList: no assignment chunks"
           ps:_  ->  fst . unzip <$> ps
    vssPhs :: WithPlaceholderOffsets [[AssignTerm]]
    vssPhs = traverse (fmap (snd . unzip)) pss

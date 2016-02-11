{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- |
-- Module      : Database.HDBC.Record.InternalTH
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides internal definitions used from DB-record templates.
module Database.HDBC.Record.InternalTH (
  -- * Persistable instances along with 'Convertible' instances
  derivePersistableInstancesFromConvertibleSqlValues
  ) where

import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Haskell.TH
  (Q, Dec (InstanceD), Type(AppT, ConT),
   Info (ClassI), reify)
import Data.Convertible (Convertible)
import Database.HDBC (SqlValue)
import Database.HDBC.SqlValueExtra ()
import Database.Record (PersistableWidth)
import Database.Record.TH (deriveNotNullType)
import Database.Record.Instances ()
import Database.Relational.Query.TH (defineScalarDegree)

import Database.HDBC.Record.TH (derivePersistableInstanceFromConvertible)


-- | Wrapper type which represents type constructor.
newtype TypeCon = TypeCon { unTypeCon :: Type } deriving Eq

-- | Ord instance for type constructor.
instance Ord TypeCon  where
  TypeCon (ConT an) `compare` TypeCon (ConT bn)    = an `compare` bn
  TypeCon (ConT _)  `compare` TypeCon _            = LT
  TypeCon _         `compare` TypeCon (ConT _)     = GT
  a                 `compare` b       | a == b     = EQ
                                      | otherwise  = EQ

-- | Set of 'TypeCon'.
type TConSet = Set TypeCon

-- | From 'Type' list into 'TConSet'.
fromList :: [Type] -> TConSet
fromList =  Set.fromList . map TypeCon

-- | From 'TConSet' into 'Type' list.
toList :: TConSet -> [Type]
toList =  map unTypeCon . Set.toList


-- | 'SqlValue' type 'Q'.
sqlValueType :: Q Type
sqlValueType =  [t| SqlValue |]

-- | 'Convertble' pairs with 'SqlValue'.
convertibleSqlValues' :: Q [(Type, Type)]
convertibleSqlValues' =  cvInfo >>= d0  where
  cvInfo = reify ''Convertible
  unknownDeclaration =
    fail . ("convertibleSqlValues: Unknown declaration pattern: " ++)
  d0 (ClassI _ is) = fmap catMaybes . mapM d1 $ is  where
    d1 (InstanceD _cxt (AppT (AppT (ConT _n) a) b) _ds)
      = do qvt <- sqlValueType
           return
             $ if qvt == a || qvt == b
               then case (a, b) of
                 (ConT _, ConT _) -> Just (a, b)
                 _                -> Nothing
               else Nothing
    d1 decl
      =    unknownDeclaration $ show decl
  d0 cls           = unknownDeclaration $ show cls

-- | Get types which are 'Convertible' with.
convertibleSqlValues :: Q TConSet
convertibleSqlValues =  do
  qvt <- sqlValueType
  vs  <- convertibleSqlValues'
  let from = fromList . map snd . filter ((== qvt) . fst) $ vs
      to   = fromList . map fst . filter ((== qvt) . snd) $ vs
  return $ Set.intersection from to

-- | Get types which are instance of 'PersistableWith'.
persistableWidthTypes :: Q TConSet
persistableWidthTypes =  cvInfo >>= d0  where
  cvInfo = reify ''PersistableWidth
  unknownDeclaration =
    fail . ("persistableWidthTypes: Unknown declaration pattern: " ++)
  d0 (ClassI _ is) = fmap fromList . mapM d1 $ is  where
    d1 (InstanceD _cxt (AppT (ConT _n) a) _ds) = return a
    d1 decl                                    = unknownDeclaration $ show decl
  d0 cls           = unknownDeclaration $ show cls

-- | Map instance declarations.
mapInstanceD :: (Q Type -> Q [Dec]) -- ^ Template to declare instances from a type
             -> [Type]              -- ^ Types
             -> Q [Dec]             -- ^ Result declaration template.
mapInstanceD fD = fmap concat . mapM (fD . return)

-- | Template to declare HDBC instances of DB-record along with 'Convertible' instances.
derivePersistableInstancesFromConvertibleSqlValues :: Q [Dec]
derivePersistableInstancesFromConvertibleSqlValues =  do
  wds <- persistableWidthTypes
  svs <- convertibleSqlValues
  ws <- mapInstanceD deriveNotNullType (toList $ Set.difference svs wds)
  let svl = toList svs
  ps <- mapInstanceD derivePersistableInstanceFromConvertible svl
  ss <- mapInstanceD defineScalarDegree svl
  return $ ws ++ ps ++ ss

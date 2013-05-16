{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Database.HDBC.Record.InternalTH (
  derivePersistableInstancesFromConvertibleSqlValues
  ) where

import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Haskell.TH
  (Q, Dec (InstanceD), Type(AppT, ConT),
   Info (ClassI), reify)
import Language.Haskell.TH.Name.Extra (compileError)
import Data.Convertible (Convertible)
import Database.HDBC (SqlValue)
import Database.HDBC.SqlValueExtra ()
import Database.Record (PersistableWidth)
import Database.Record.TH (deriveNotNullType)
import Database.Record.Instances ()

import Database.HDBC.Record.TH (derivePersistableInstanceFromValue)


newtype TypeCon = TypeCon { unTypeCon :: Type } deriving Eq

instance Ord TypeCon  where
  TypeCon (ConT an) `compare` TypeCon (ConT bn)    = an `compare` bn
  TypeCon (ConT _)  `compare` TypeCon _            = LT
  TypeCon _         `compare` TypeCon (ConT _)     = GT
  a                 `compare` b       | a == b     = EQ
                                      | otherwise  = EQ

type TConSet = Set TypeCon

fromList :: [Type] -> TConSet
fromList =  Set.fromList . map TypeCon

toList :: TConSet -> [Type]
toList =  map unTypeCon . Set.toList


sqlValueType :: Q Type
sqlValueType =  [t| SqlValue |]

convertibleSqlValues' :: Q [(Type, Type)]
convertibleSqlValues' =  cvInfo >>= d0  where
  cvInfo = reify ''Convertible
  unknownDeclaration = compileError
                       . ("convertibleSqlValues: Unknown declaration pattern: " ++)
  d0 (ClassI _ is) = fmap catMaybes . sequence . map d1 $ is  where
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

convertibleSqlValues :: Q TConSet
convertibleSqlValues =  do
  qvt <- sqlValueType
  vs  <- convertibleSqlValues'
  let from = fromList . map snd . filter ((== qvt) . fst) $ vs
      to   = fromList . map fst . filter ((== qvt) . snd) $ vs
  return $ Set.intersection from to

persistableWidthValues :: Q TConSet
persistableWidthValues =  cvInfo >>= d0  where
  cvInfo = reify ''PersistableWidth
  unknownDeclaration = compileError
                       . ("persistableWidthValues: Unknown declaration pattern: " ++)
  d0 (ClassI _ is) = fmap fromList . sequence . map d1 $ is  where
    d1 (InstanceD _cxt (AppT (ConT _n) a) _ds) = return a
    d1 decl                                    = unknownDeclaration $ show decl
  d0 cls           = unknownDeclaration $ show cls

mapInstanceD :: (Q Type -> Q [Dec]) -> [Type] -> Q [Dec]
mapInstanceD fD = fmap concat . mapM (fD . return)

derivePersistableInstancesFromConvertibleSqlValues :: Q [Dec]
derivePersistableInstancesFromConvertibleSqlValues =  do
  wds <- persistableWidthValues
  svs <- convertibleSqlValues
  ws <- mapInstanceD deriveNotNullType (toList $ Set.difference svs wds)
  ps <- mapInstanceD derivePersistableInstanceFromValue (toList svs)
  return $ ws ++ ps

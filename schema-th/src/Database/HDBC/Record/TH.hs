{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Database.HDBC.Record.TH (
  derivePersistableInstanceFromValue,
  derivePersistableInstancesFromConvertibleSqlValues
  ) where

import Data.Maybe (catMaybes)
import Data.List (intersect)

import Language.Haskell.TH
  (Q, Dec (InstanceD), Type(AppT, ConT),
   Info (ClassI), reify, runIO)
import Data.Convertible (Convertible)
import Database.HDBC (SqlValue)
import Database.Record.Persistable
  (Persistable(persistable), derivedPersistableRecord, PersistableWidth(persistableWidth))
import qualified Database.Record.Persistable as Persistable


sqlValueType :: Q Type
sqlValueType =  [t| SqlValue |]

convertibleSqlValues' :: Q [(Type, Type)]
convertibleSqlValues' =  cvInfo >>= d0  where
  cvInfo = reify ''Convertible
  unknownDeclaration = runIO . ioError . userError
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

convertibleSqlValues :: Q [Type]
convertibleSqlValues =  do
  qvt <- sqlValueType
  vs  <- convertibleSqlValues'
  let from = map snd . filter ((== qvt) . fst) $ vs
      to   = map fst . filter ((== qvt) . snd) $ vs
  return $ intersect from to

derivePersistableInstanceFromValue :: Q Type -> Q [Dec]
derivePersistableInstanceFromValue typ =
  [d| instance PersistableWidth $(typ)  where
        persistableWidth = Persistable.valueWidth

      instance Persistable SqlValue $(typ)  where
        persistable = derivedPersistableRecord
    |]

derivePersistableInstancesFromConvertibleSqlValues :: Q [Dec]
derivePersistableInstancesFromConvertibleSqlValues =  do
  ts <- convertibleSqlValues
  concat `fmap` mapM (derivePersistableInstanceFromValue . return) ts

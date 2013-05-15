{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Database.HDBC.Record.TH (
  derivePersistableInstanceFromValue,
  derivePersistableInstancesFromConvertibleSqlValues
  ) where

import Data.Maybe (catMaybes)
import Data.List (intersect, find)

import Language.Haskell.TH
  (Q, Dec (InstanceD), Type(AppT, ConT),
   Info (ClassI), reify)
import Language.Haskell.TH.Name.Extra (compileError)
import Data.Convertible (Convertible)
import Database.HDBC (SqlValue)
import Database.HDBC.SqlValueExtra ()
import Database.Record
  (Persistable(persistable), derivedPersistableValueRecord, PersistableWidth(persistableWidth),
   FromSql(recordFromSql), recordFromSql',
   ToSql(recordToSql), recordToSql')
import Database.Record.Instances ()
import qualified Database.Record.Persistable as Persistable


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

convertibleSqlValues :: Q [Type]
convertibleSqlValues =  do
  qvt <- sqlValueType
  vs  <- convertibleSqlValues'
  let from = map snd . filter ((== qvt) . fst) $ vs
      to   = map fst . filter ((== qvt) . snd) $ vs
  return $ intersect from to

persistableWidthValues :: Q [Type]
persistableWidthValues =  cvInfo >>= d0  where
  cvInfo = reify ''PersistableWidth
  unknownDeclaration = compileError
                       . ("persistableWidthValues: Unknown declaration pattern: " ++)
  d0 (ClassI _ is) = sequence . map d1 $ is  where
    d1 (InstanceD _cxt (AppT (ConT _n) a) _ds) = return a
    d1 decl                                    = unknownDeclaration $ show decl
  d0 cls           = unknownDeclaration $ show cls

derivePersistableWidth :: Q Type -> Q [Dec]
derivePersistableWidth typ =
  [d| instance PersistableWidth $(typ)  where
        persistableWidth = Persistable.valueWidth
    |]

derivePersistableInstanceFromValue :: Q Type -> Q [Dec]
derivePersistableInstanceFromValue typ =
  [d| instance Persistable SqlValue $(typ)  where
        persistable = derivedPersistableValueRecord

      instance FromSql SqlValue $(typ)  where
        recordFromSql = recordFromSql'

      instance ToSql SqlValue $(typ)  where
        recordToSql = recordToSql'
    |]

mapInstanceD :: (Q Type -> Q [Dec]) -> [Type] -> Q [Dec]
mapInstanceD fD = fmap concat . mapM (fD . return)

derivePersistableInstancesFromConvertibleSqlValues :: Q [Dec]
derivePersistableInstancesFromConvertibleSqlValues =  do
  ds <- persistableWidthValues
  ts <- convertibleSqlValues
  let defineNotDefined qt = do
        t <- qt
        case find (== t) ds of
          Nothing -> derivePersistableWidth qt
          Just _  -> return []
  ws <- mapInstanceD defineNotDefined ts
  ps <- mapInstanceD derivePersistableInstanceFromValue ts
  return $ ws ++ ps

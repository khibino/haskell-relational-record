{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}

module Database.Record.InternalTH (
  definePersistableWidthInstance,
  defineSqlPersistableInstances,
  defineTupleInstances,
  knownWidthIntType,
  ) where

import Control.Applicative ((<$>))
import Data.Int (Int32, Int64)
import Language.Haskell.TH
  (Q, mkName, Name, tupleTypeName,
   TypeQ, varT, Dec, instanceD, )
import Language.Haskell.TH.Compat.Constraint (classP)
import Data.Functor.ProductIsomorphic.TH (reifyRecordType)

import Database.Record.Persistable (PersistableWidth)
import Database.Record.FromSql (FromSql)
import Database.Record.ToSql (ToSql)


-- | Polymorphic 'PersistableWidth' instance template.
definePersistableWidthInstance :: TypeQ   -- ^ Record type construct expression.
                               -> [Name]  -- ^ Record type construct argument variables.
                               -> Q [Dec] -- ^ Definition of 'PersistableWidth' instance.
definePersistableWidthInstance tyCon avs  = do
  -- in template-haskell 2.8 or older, Pred is not Type
  let classP' n v = classP n [varT v]
  (:[]) <$>
    instanceD
    (mapM (classP' ''PersistableWidth) avs)
    [t| PersistableWidth $tyCon |] []

-- | Polymorphic record parser and printer instance templates
--   for converting between list of SQL type and Haskell record type.
defineSqlPersistableInstances :: TypeQ
                              -> TypeQ
                              -> [Name]
                              -> Q [Dec]
defineSqlPersistableInstances tySql tyRec avs = do
  -- in template-haskell 2.8 or older, Pred is not Type
  let classP' n v = classP n [tySql, varT v]
  fromI <-
    instanceD
    (mapM (classP' ''FromSql) avs)
    [t| FromSql $tySql $tyRec |] []
  toI   <-
    instanceD
    (mapM (classP' ''ToSql) avs)
    [t| ToSql $tySql $tyRec |] []
  return [fromI, toI]

persistableWidth :: Int -> Q [Dec]
persistableWidth n = do
  (((tyCon, avs), _), _) <- reifyRecordType $ tupleTypeName n
  definePersistableWidthInstance tyCon avs

sqlInstances :: Int -> Q [Dec]
sqlInstances n = do
  (((tyCon, avs), _), _) <- reifyRecordType $ tupleTypeName n
  defineSqlPersistableInstances (varT $ mkName "q") tyCon avs

-- | Template to define tuple instances of persistable-record classes.
defineTupleInstances :: Int -> Q [Dec]
defineTupleInstances n =
  concat <$> sequence
  [ persistableWidth n, sqlInstances n ]

knownWidthIntType :: Maybe TypeQ
knownWidthIntType
  | toI (minBound :: Int) == toI (minBound :: Int32) &&
    toI (maxBound :: Int) == toI (maxBound :: Int32)    =  Just [t| Int |]
  | toI (minBound :: Int) == toI (minBound :: Int64) &&
    toI (maxBound :: Int) == toI (maxBound :: Int64)    =  Just [t| Int |]
  | otherwise                                           =  Nothing
  where
    toI :: Integral a => a -> Integer
    toI = fromIntegral

{-# LANGUAGE TemplateHaskell #-}

module Database.Record.InternalTH (
  defineTupleInstances
  ) where

import Control.Applicative ((<$>), pure)
import Control.Monad (replicateM)
import Data.List (foldl')
import Language.Haskell.TH
  (Q,  newName, Name,
   conT, varT, tupleT, appT, classP,
   Dec, instanceD, )

import Database.Record.Persistable (PersistableWidth)
import Database.Record.FromSql (FromSql)
import Database.Record.ToSql (ToSql)


tupleInstance :: Int -> Name -> Q [Dec]
tupleInstance n clazz = do
  ns <- replicateM n (newName "t")
  (:[]) <$> instanceD
    (mapM (classP clazz . (:[]) . varT) ns)
    (appT (conT clazz) . foldl' appT (tupleT n) $ map varT ns)
    []

tupleInstance2 :: Int -> Name -> Q [Dec]
tupleInstance2 n clazz = do
  ns <- replicateM n (newName "t")
  q <- varT =<< newName "q"
  let qT = pure q -- same var q
  (:[]) <$> instanceD
    (mapM (classP clazz . ([qT] ++) . (:[]) . varT) ns)
    (appT (appT (conT clazz) qT) . foldl' appT (tupleT n) $ map varT ns)
    []

-- | Template to define tuple instances of persistable-record classes.
defineTupleInstances :: Int -> Q [Dec]
defineTupleInstances n =
  concat <$> sequence
  [ tupleInstance  n ''PersistableWidth
  , tupleInstance2 n ''FromSql
  , tupleInstance2 n ''ToSql]

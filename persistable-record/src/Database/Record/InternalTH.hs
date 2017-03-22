{-# LANGUAGE TemplateHaskell #-}

module Database.Record.InternalTH (
  defineTupleInstances
  ) where

import Control.Applicative ((<$>))
import Data.List (foldl')
import Language.Haskell.TH
  (Q, mkName, Name,
   conT, varT, tupleT, appT, classP,
   Dec, instanceD, )

import Database.Record.Persistable (PersistableWidth)
import Database.Record.FromSql (FromSql)
import Database.Record.ToSql (ToSql)


persistableWidth :: Int -> Q [Dec]
persistableWidth n = do
  let vs = [ varT . mkName $ "a" ++ show i | i <- [1 .. n] ]
  (:[]) <$> instanceD
    -- in template-haskell 2.8 or older, Pred is not Type
    (mapM (classP ''PersistableWidth . (:[])) vs)
    [t| PersistableWidth $(foldl' appT (tupleT n) vs) |]
    []

tupleInstance2 :: Int -> Name -> Q [Dec]
tupleInstance2 n clazz = do
  let vs = [ varT . mkName $ "a" ++ show i | i <- [1 .. n] ]
      q = varT $ mkName "q"
  (:[]) <$> instanceD
    -- in template-haskell 2.8 or older, Pred is not Type
    (mapM (\v -> classP clazz [q, v]) vs)
    [t| $(conT clazz) $q $(foldl' appT (tupleT n) vs) |]
    []

-- | Template to define tuple instances of persistable-record classes.
defineTupleInstances :: Int -> Q [Dec]
defineTupleInstances n =
  concat <$> sequence
  [ persistableWidth n
  , tupleInstance2 n ''FromSql
  , tupleInstance2 n ''ToSql ]

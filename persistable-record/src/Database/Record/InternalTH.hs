{-# LANGUAGE TemplateHaskell #-}

module Database.Record.InternalTH (
  defineTupleInstances
  ) where

import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Data.List (foldl')
import Language.Haskell.TH
  (Q,  newName, Name,
   conT, varT, tupleT, appT, classP,
   Dec, instanceD, )

import Database.Record.Persistable (PersistableWidth)
import Database.Record.FromSql (FromSql)
import Database.Record.ToSql (ToSql)


persistableWidth :: Int -> [Name] -> Q [Dec]
persistableWidth n ns = do
  let vs = map varT ns -- same vars vs
  (:[]) <$> instanceD
    -- in template-haskell 2.8 or older, Pred is not Type
    (mapM (classP ''PersistableWidth . (:[])) vs)
    [t| PersistableWidth $(foldl' appT (tupleT n) vs) |]
    []

tupleInstance2 :: Int -> Name -> [Name] -> Name -> Q [Dec]
tupleInstance2 n q ns clazz = do
  let vs = map varT ns -- same vars vs
      qT = varT q -- same var q
  (:[]) <$> instanceD
    -- in template-haskell 2.8 or older, Pred is not Type
    (mapM (\v -> classP clazz [qT, v]) vs)
    [t| $(conT clazz) $qT $(foldl' appT (tupleT n) vs) |]
    []

-- | Template to define tuple instances of persistable-record classes.
defineTupleInstances :: Int -> Q [Dec]
defineTupleInstances n = do
  ns <- replicateM n $ newName "a"
  q  <- newName "q"
  concat <$> sequence
    [ persistableWidth n ns
    , tupleInstance2 n q ns ''FromSql
    , tupleInstance2 n q ns ''ToSql]

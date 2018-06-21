{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

-- |
-- Module      : Database.Relational.InternalTH.Overloaded
-- Copyright   : 2017-2018 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines overloaded projection templates for internally using.
module Database.Relational.InternalTH.Overloaded (
  monomorphicProjection,
  polymorphicProjections,
  tupleProjection,
  definePrimaryHasProjection,
  ) where

#if __GLASGOW_HASKELL__ >= 800
import Language.Haskell.TH
  (Name, mkName, Q, TypeQ, Dec, instanceD, funD, classP,
   appT, tupleT, varT, litT, strTyLit, clause, normalB, listE)
import Language.Haskell.TH.Lib.Extra (integralE)
import Language.Haskell.TH.Name.CamelCase
  (ConName, conName, toVarExp, toTypeCon)
import Data.List (foldl', inits)
import Data.Array ((!))
import Database.Record.Persistable
  (PersistableWidth, persistableWidth,
   PersistableRecordWidth, runPersistableRecordWidth)
import Database.Record.TH (columnOffsetsVarNameDefault)

import Database.Relational.Pi.Unsafe (definePi)
import Database.Relational.Constraint (unsafeDefineConstraintKey, projectionKey)
import Database.Relational.OverloadedProjection (HasProjection (projection))
#else
import Language.Haskell.TH (Name, mkName, Q, TypeQ, appT, tupleT, varT, Dec)
import Language.Haskell.TH.Name.CamelCase (ConName)
import Data.List (foldl')
#endif

-- | Projection template for monomorphic record type.
monomorphicProjection :: ConName
                      -> String
                      -> Int
                      -> TypeQ
                      -> Q [Dec]
#if __GLASGOW_HASKELL__ >= 800
monomorphicProjection recName colStr ix colType =
    [d| instance HasProjection $(litT $ strTyLit colStr) $(toTypeCon recName) $colType where
          projection _ = definePi $ $offsetsExp ! $(integralE ix)
      |]
  where
    offsetsExp = toVarExp . columnOffsetsVarNameDefault $ conName recName
#else
monomorphicProjection _ _ _ _ = [d| |]
#endif

-- | Projection templates for record type with type variable.
polymorphicProjections :: TypeQ
                       -> [Name]
                       -> [String]
                       -> [TypeQ]
                       -> Q [Dec]
#if __GLASGOW_HASKELL__ >= 800
polymorphicProjections recType avs sels cts =
    sequence $ zipWith3 template sels cts (inits cts)
  where
    template colStr colType pcts =
      instanceD
      (mapM (classP ''PersistableWidth . (:[]) . varT) avs)
      [t| HasProjection $(litT $ strTyLit colStr) $recType $colType |]
      [projectionDec pcts]

projectionDec :: [TypeQ] -> Q Dec
projectionDec cts =
    funD
    (mkName "projection")
    [clause [[p| _ |]]
      (normalB [| definePi $(foldl' (\e t -> [| $e + $(runPW t) |]) [| 0 :: Int |] cts) |])
      []]
  --- In sub-tree, newName "projection" is called by [d| projection .. = |]?
  --- head <$> [d| projection _ =  definePi $(foldl' (\e t -> [| $e + $(runPW t) |]) [| 0 :: Int |] cts) |]
  where
    runPW t = [| runPersistableRecordWidth (persistableWidth :: PersistableRecordWidth $t) |]
#else
polymorphicProjections _ _ _ _ = [d| |]
#endif

-- | Projection templates for tuple type.
tupleProjection :: Int -> Q [Dec]
tupleProjection n = do
    p <- polymorphicProjections tyRec avs ["fst", "snd"] cts
    q <- polymorphicProjections tyRec avs sels cts
    r <- polymorphicProjections tyRec avs oldSels cts -- DEPRECATED. drop in 0.12.0
    return $ p ++ q ++ r
  where
    sels = [ "pi" ++ show i
           | i <- [ 0 .. n - 1] ]
    oldSels = [ "tuplePi" ++ show n ++ "_" ++ show i
              | i <- [ 0 .. n - 1] ]
    ((avs, cts), tyRec) = tupleN
    tupleN :: (([Name], [TypeQ]), TypeQ)
    --- same as tupleN of InternalTH.Base, merge after dropping GHC 7.x
    tupleN = ((ns, vs), foldl' appT (tupleT n) vs)
      where
        ns = [ mkName $ "a" ++ show j | j <- [1 .. n] ]
        vs = map varT ns

-- | Projection template for primary key.
definePrimaryHasProjection :: TypeQ   -- ^ Record type
                           -> TypeQ   -- ^ Key type
                           -> [Int]   -- ^ Indexes specifies key
                           -> Q [Dec] -- ^ Result 'HasProjection' declaration
#if __GLASGOW_HASKELL__ >= 800
definePrimaryHasProjection recType colType indexes =
  [d| instance HasProjection "primary" $recType $colType  where
        projection _ = projectionKey
                       $ unsafeDefineConstraintKey $(listE [integralE ix | ix <- indexes])
    |]
#else
definePrimaryHasProjection _ _ _ = [d| |]
#endif

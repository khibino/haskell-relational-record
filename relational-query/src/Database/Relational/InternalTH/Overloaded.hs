{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Database.Relational.InternalTH.Overloaded
-- Copyright   : 2017 Kei Hibino
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
  ) where

#if __GLASGOW_HASKELL__ >= 800
import Language.Haskell.TH
  (Name, Q, TypeQ, Dec, instanceD, classP, varT, litT, strTyLit)
import Language.Haskell.TH.Lib.Extra (integralE)
import Language.Haskell.TH.Name.CamelCase
  (ConName, conName, toVarExp, toTypeCon)
import Data.List (foldl', inits)
import Data.Array ((!))
import Database.Record.Persistable (PersistableWidth, PersistableRecordWidth)
import Database.Record.TH (columnOffsetsVarNameDefault)

import Database.Relational.Pi.Unsafe (definePi)
import Database.Relational.OverloadedProjection (HasProjection (..))
#else
import Language.Haskell.TH (Name, Q, TypeQ, Dec)
import Language.Haskell.TH.Name.CamelCase (ConName)
#endif

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

polymorphicProjections :: TypeQ
                       -> [Name]
                       -> [String]
                       -> [TypeQ]
                       -> Q [Dec]
#if __GLASGOW_HASKELL__ >= 800
polymorphicProjections recType avs sels cts =
    sequence $ zipWith3 template sels cts (inits cts)
  where
    runPW t = [| runPersistableRecordWidth (persistableWidth :: PersistableRecordWidth $t) |]
    template colStr colType pcts =
      instanceD
      (mapM (classP ''PersistableWidth . (:[]) . varT) avs)
      [t| HasProjection $(litT $ strTyLit colStr) $recType $colType |]
      [ head <$> [d| projection _ =  definePi $(foldl' (\e t -> [| $e + $(runPW t) |]) [| 0 :: Int |] pcts) |] ]
#else
polymorphicProjections _ _ _ _ = [d| |]
#endif

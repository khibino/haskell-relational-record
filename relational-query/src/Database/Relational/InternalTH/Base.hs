{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}

-- |
-- Module      : Database.Relational.InternalTH.Base
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines templates for internally using.
module Database.Relational.InternalTH.Base (
  defineTupleShowLiteralInstance,
  defineTuplePi,

  defineRecordProjections,
  ) where

import Control.Applicative ((<$>))
import Data.List (foldl', inits)
import Language.Haskell.TH
  (Q, Name, mkName, normalB, varP,
   TypeQ, forallT, varT, tupleT, appT,
   Dec, sigD, valD, instanceD,
   TyVarBndr (PlainTV),
   #if MIN_VERSION_bse(4,15,0)
   Specificity(..)
   #endif
   )
import Language.Haskell.TH.Compat.Constraint (classP)
import Database.Record.Persistable
  (PersistableWidth, persistableWidth,
   PersistableRecordWidth, runPersistableRecordWidth)

import Database.Relational.ProjectableClass (LiteralSQL (..))
import Database.Relational.Pi.Unsafe (Pi, definePi)

#if MIN_VERSION_base(4,15,0)
plainTV :: Name -> TyVarBndr
plainTV = PlainTV
#else
plainTV :: Name -> TyVarBndr Specificity
plainTV n = PlainTV n SpecifiedSpec
#endif
tupleN :: Int -> (([Name], [TypeQ]), TypeQ)
tupleN n = ((ns, vs), foldl' appT (tupleT n) vs)
  where
    ns = [ mkName $ "a" ++ show j | j <- [1 .. n] ]
    vs = map varT ns

-- | Make template of 'LiteralSQL' instance of tuple type.
defineTupleShowLiteralInstance :: Int -> Q [Dec]
defineTupleShowLiteralInstance n = do
  let ((_, vs), tty)  =  tupleN n
  (:[]) <$> instanceD
    -- in template-haskell 2.8 or older, Pred is not Type
    (mapM (classP ''LiteralSQL . (:[])) vs)
    [t| LiteralSQL $tty |]
    []

-- | Make polymorphic projection templates.
defineRecordProjections :: TypeQ -> [Name] -> [Name] -> [TypeQ] -> Q [Dec]
defineRecordProjections tyRec avs sels cts =
    fmap concat . sequence $ zipWith3 template cts (inits cts) sels
  where
    template :: TypeQ -> [TypeQ] -> Name -> Q [Dec]
    template ct pcts selN = do
      sig <- sigD selN $
             forallT (map plainTV avs)
             (mapM (classP ''PersistableWidth . (:[]) . varT) avs)
             [t| Pi $tyRec $ct |]
      let runPW t = [| runPersistableRecordWidth (persistableWidth :: PersistableRecordWidth $t) |]
      val <- valD (varP selN)
             (normalB [| definePi $(foldl' (\e t -> [| $e + $(runPW t) :: Int |]) [| 0 :: Int |] pcts) |]) []
      return [sig, val]

-- | Make templates of projection paths for tuple types.
defineTuplePi :: Int -> Q [Dec]
defineTuplePi n =
  defineRecordProjections tyRec avs sels cts
  where
    ((avs, cts), tyRec) = tupleN n
    sels = [ mkName $ "tuplePi" ++ show n ++ "_" ++ show i ++ "'"
           | i <- [ 0 .. n - 1] ]

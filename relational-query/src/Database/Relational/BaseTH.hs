{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

-- |
-- Module      : Database.Relational.BaseTH
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines templates for internally using.
module Database.Relational.BaseTH (
  defineTupleShowConstantInstance,
  defineTuplePi,
  ) where

import Control.Applicative ((<$>))
import Data.List (foldl')
import Language.Haskell.TH
  (Q, Name, mkName, normalB, classP, varP,
   TypeQ, forallT, varT, tupleT, appT,
   Dec, sigD, valD, instanceD,
   TyVarBndr (PlainTV), )
import Database.Record.Persistable
  (PersistableWidth, persistableWidth,
   PersistableRecordWidth, runPersistableRecordWidth)

import Database.Relational.ProjectableClass (ShowConstantTermsSQL (..))
import Database.Relational.Pi.Unsafe (Pi, definePi)


tupleN :: Int -> (([Name], [TypeQ]), TypeQ)
tupleN n = ((ns, vs), foldl' appT (tupleT n) vs)
  where
    ns = [ mkName $ "a" ++ show j | j <- [1 .. n] ]
    vs = map varT ns

-- | Make template of 'ShowConstantTermsSQL' instance of tuple type.
defineTupleShowConstantInstance :: Int -> Q [Dec]
defineTupleShowConstantInstance n = do
  let ((_, vs), tty)  =  tupleN n
  (:[]) <$> instanceD
    -- in template-haskell 2.8 or older, Pred is not Type
    (mapM (classP ''ShowConstantTermsSQL . (:[])) vs)
    [t| ShowConstantTermsSQL $tty |]
    []

tuplePi :: Int -> Int -> Q [Dec]
tuplePi n i = do
  let selN = mkName $ "tuplePi" ++ show n ++ "_" ++ show i ++ "'"
      ((ns, vs), tty) = tupleN n
  sig <- sigD selN $
         forallT (map PlainTV ns)
         (mapM (classP ''PersistableWidth . (:[])) vs)
         [t| Pi $tty $(vs !! i) |]
  val <- valD (varP selN)
         (normalB [| definePi $(foldl'
                                (\e t -> [| $e + runPersistableRecordWidth (persistableWidth :: PersistableRecordWidth $t) |])
                                [| 0 :: Int |] $ take i vs) |])
         []
  return [sig, val]

-- | Make templates of projection paths for tuple types.
defineTuplePi :: Int -> Q [Dec]
defineTuplePi n =
  concat <$> mapM (tuplePi n) [0 .. n - 1]

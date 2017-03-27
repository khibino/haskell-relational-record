{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Relational.Query.Internal.TH (
  defineProductConstructorInstance,
  defineTuplePi,
  ) where

import Control.Applicative ((<$>))
import Data.List (foldl')
import Language.Haskell.TH
  (Q, mkName, normalB, classP, varP,
   Exp,
   Type, forallT, varT, tupleT, appT, arrowT,
   Dec, sigD, valD,
   TyVarBndr (PlainTV), )
import Database.Record.Persistable
  (PersistableWidth, persistableWidth,
   PersistableRecordWidth, runPersistableRecordWidth)

import Database.Relational.Query.Internal.ProjectableClass (ProductConstructor (..))

import Database.Relational.Query.Pi.Unsafe (Pi, definePi)


-- | Make template for 'ProductConstructor' instance.
defineProductConstructorInstance :: Q Type -> Q Exp -> [Q Type] -> Q [Dec]
defineProductConstructorInstance recTypeQ recData colTypes =
  [d| instance ProductConstructor $(foldr (appT . (arrowT `appT`)) recTypeQ colTypes) where
        productConstructor = $(recData)
    |]

-- | xxx
tuplePi :: Int -> Int -> Q [Dec]
tuplePi n i = do
  let selN = mkName $ "tuplePi" ++ show n ++ "_" ++ show i ++ "'"
      ns = [ mkName $ "a" ++ show j | j <- [1 .. n] ]
      vs = map varT ns
  sig <- sigD selN $
         forallT (map PlainTV ns)
         (mapM (classP ''PersistableWidth . (:[])) vs)
         [t| Pi $(foldl' appT (tupleT n) vs) $(vs !! i) |]
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

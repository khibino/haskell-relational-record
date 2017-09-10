{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}

-- |
-- Module      : Database.Relational.ProjectableClass
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides interfaces to preserve constraints of
-- direct product projections.
module Database.Relational.ProjectableClass (
  -- * Literal SQL terms
  ShowConstantTermsSQL (..), showConstantTermsSQL,
  StringSQL,

  ) where

import GHC.Generics (Generic, Rep, U1 (..), K1 (..), M1 (..), (:*:)(..), from)
import Data.Monoid (mempty, (<>))
import Data.DList (DList, toList)

import Database.Relational.Internal.String (StringSQL)


-- | Convert from haskell record to SQL terms list.
showConstantTermsSQL :: ShowConstantTermsSQL a
                     => a
                     -> [StringSQL]
showConstantTermsSQL = toList . showConstantTermsSQL'

{- |
'ShowConstantTermsSQL' 'a' is implicit rule to derive function to convert
from haskell record type 'a' into constant SQL terms.

Generic programming (<https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#generic-programming>)
with default signature is available for 'ShowConstantTermsSQL' class,
so you can make instance like below:

@
  \{\-\# LANGUAGE DeriveGeneric \#\-\}
  import GHC.Generics (Generic)
  --
  data Foo = Foo { ... } deriving Generic
  instance ShowConstantTermsSQL Foo
@

-}
class ShowConstantTermsSQL a where
  showConstantTermsSQL' :: a -> DList StringSQL

  default showConstantTermsSQL' :: (Generic a, GShowConstantTermsSQL (Rep a)) => a -> DList StringSQL
  showConstantTermsSQL' = gShowConstantTermsSQL . from

class GShowConstantTermsSQL f where
  gShowConstantTermsSQL :: f a -> DList StringSQL

instance GShowConstantTermsSQL U1 where
  gShowConstantTermsSQL U1 = mempty

instance (GShowConstantTermsSQL a, GShowConstantTermsSQL b) =>
         GShowConstantTermsSQL (a :*: b) where
  gShowConstantTermsSQL (a :*: b) = gShowConstantTermsSQL a <> gShowConstantTermsSQL b

instance GShowConstantTermsSQL a => GShowConstantTermsSQL (M1 i c a) where
  gShowConstantTermsSQL (M1 a) = gShowConstantTermsSQL a

instance ShowConstantTermsSQL a => GShowConstantTermsSQL (K1 i a) where
  gShowConstantTermsSQL (K1 a) = showConstantTermsSQL' a

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ConstraintKinds #-}

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
  LiteralSQL (..), showLiteral,
  StringSQL,

  -- * Deprecated.
  ShowConstantTermsSQL, showConstantTermsSQL', showConstantTermsSQL,
  ) where

import GHC.Generics (Generic, Rep, U1 (..), K1 (..), M1 (..), (:*:)(..), from)
import Data.Monoid (mempty, (<>))
import Data.DList (DList, toList)

import Database.Relational.Internal.String (StringSQL)


-- | Convert from haskell record to SQL literal row-value.
showLiteral :: LiteralSQL a
            => a
            -> [StringSQL]
showLiteral = toList . showLiteral'

{- |
'LiteralSQL' 'a' is implicit rule to derive function to convert
from haskell record type 'a' into SQL literal row-value.

Generic programming (<https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#generic-programming>)
with default signature is available for 'LiteralSQL' class,
so you can make instance like below:

@
  \{\-\# LANGUAGE DeriveGeneric \#\-\}
  import GHC.Generics (Generic)
  --
  data Foo = Foo { ... } deriving Generic
  instance LiteralSQL Foo
@

-}
class LiteralSQL a where
  showLiteral' :: a -> DList StringSQL

  default showLiteral' :: (Generic a, GLiteralSQL (Rep a)) => a -> DList StringSQL
  showLiteral' = gShowLiteral . from

class GLiteralSQL f where
  gShowLiteral :: f a -> DList StringSQL

instance GLiteralSQL U1 where
  gShowLiteral U1 = mempty

instance (GLiteralSQL a, GLiteralSQL b) =>
         GLiteralSQL (a :*: b) where
  gShowLiteral (a :*: b) = gShowLiteral a <> gShowLiteral b

instance GLiteralSQL a => GLiteralSQL (M1 i c a) where
  gShowLiteral (M1 a) = gShowLiteral a

instance ShowConstantTermsSQL a => GLiteralSQL (K1 i a) where
  gShowLiteral (K1 a) = showLiteral' a

---

{-# DEPRECATED ShowConstantTermsSQL "Use `LiteralSQL` instead of this." #-}
-- | Deprecated.
type ShowConstantTermsSQL = LiteralSQL

{-# DEPRECATED showConstantTermsSQL' "Use `showLiteral'` instead of this." #-}
showConstantTermsSQL' :: ShowConstantTermsSQL a => a -> DList StringSQL
showConstantTermsSQL' = showLiteral'

{-# DEPRECATED showConstantTermsSQL "Use `showLiteral` instead of this." #-}
-- | Deprecated.
showConstantTermsSQL :: ShowConstantTermsSQL a
                     => a
                     -> [StringSQL]
showConstantTermsSQL = toList . showConstantTermsSQL'

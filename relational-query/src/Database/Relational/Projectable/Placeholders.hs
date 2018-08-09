{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Relational.Projectable.Placeholders
  ( ph
  ) where


import qualified Data.Extensible as E
import           Data.Extensible (type (>:), (@=), (<:))
import           Data.Functor.Const
import           Data.Kind
import           Database.Relational.ExtensibleRecord
import           Database.Relational.SqlSyntax (Record, previousIndexOf, setNextIndexOf)
import           Database.Relational.Projectable.Unsafe (SqlContext (..))


ph
  :: forall key v xs c (a :: Type)
   . (Show v, SqlContext c)
  => E.FieldName key
  -> Record (ExRecord xs) (ExRecord (key >: v : xs)) c a
ph field = addPlaceholder field $ unsafeProjectSqlTerms ["?"]


addPlaceholder
  :: forall key v xs c t
   . (Show v, SqlContext c)
  => E.FieldName key
  -> Record (ExRecord xs) (ExRecord xs) c t
  -> Record (ExRecord xs) (ExRecord (key >: v : xs)) c t
addPlaceholder field record = setNextIndexOf
  (field @= error "TODO: where can I get the value?" <: previousIndexOf record)
  record


-- | Copied from https://hackage.haskell.org/package/microlens-0.4.9.1/docs/Lens-Micro.html#t:Getting
type Getting r s a = (a -> Const r a) -> s -> Const r s

-- | Copied from https://hackage.haskell.org/package/microlens-0.4.9.1/docs/src/Lens-Micro.html#%5E.
(^.) :: s -> Getting a s a -> a
s ^. l = getConst (l Const s)
{-# INLINE (^.) #-}
infixl 8 ^.

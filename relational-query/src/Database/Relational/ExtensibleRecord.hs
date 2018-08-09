{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Database.Relational.ExtensibleRecord
  ( type (E.++)
  , type (E.>:)
  , ExRecord
  , ExRecordNil
  , E.happend
  , rightId
  ) where


import           Data.Kind (Type)
import           Data.Extensible (type (++))
import qualified Data.Extensible as E
import           Data.Extensible.HList (HList(HNil, HCons))
import           Data.Functor.Identity (Identity)
import           Data.Type.Equality
import           GHC.TypeLits (Symbol)


type ExRecord (kvs :: [E.Assoc Symbol Type]) = E.Record kvs

type ExRecordNil = ExRecord '[]


rightId :: ExRecord xs -> (xs ++ '[]) :~: xs
rightId = d . E.toHList
 where
  d :: HList (E.Field Identity) xs -> (xs ++ '[]) :~: xs
  d HNil = Refl
  d (HCons _x xs) = case d xs of Refl -> Refl

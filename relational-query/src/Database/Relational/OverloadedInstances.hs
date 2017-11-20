{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Database.Relational.OverloadedInstances () where

import Control.Applicative ((<$>))
#if __GLASGOW_HASKELL__ >= 800
import Database.Record.Persistable
  (PersistableWidth, persistableWidth,
   PersistableRecordWidth, runPersistableRecordWidth)

import Database.Relational.Pi.Unsafe (definePi)
import Database.Relational.OverloadedProjection (HasProjection (projection))
#endif
import Database.Relational.InternalTH.Overloaded (tupleProjection)


$(concat <$> mapM tupleProjection [2 .. 7])
-- Generic instances of tuple types are generated from 2 to 7 in GHC.Generics.

#if __GLASGOW_HASKELL__ >= 800
instance PersistableWidth a =>
         HasProjection "fst" (a, b) a where
  projection _ = definePi 0

instance (PersistableWidth a, PersistableWidth b) =>
         HasProjection "snd" (a, b) b where
  projection _ = definePi $ runPersistableRecordWidth (persistableWidth :: PersistableRecordWidth b)
#endif

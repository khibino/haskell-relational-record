{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Database.Relational.Export
-- Copyright   : 2021 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines templates to export SQL string representation to other systems.
module Database.Relational.Export (
  inlineQuery_,
  inlineUpdate_,
  inlineInsertValue_,
  inlineInsertQuery_,
  inlineDelete_,
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString.Short (ShortByteString, toShort)

import Language.Haskell.TH (Q, Dec, stringE)
import Language.Haskell.TH.Name.CamelCase (varName, varCamelcaseName)
import Language.Haskell.TH.Lib.Extra (simpleValD)

import Database.Relational
  (Query, Update, Insert, InsertQuery, Delete,
   untypeQuery, UntypeableNoFetch (untypeNoFetch))


inlineSQL_ :: (String -> Q ()) -- ^ action to check SQL string
           -> String           -- ^ SQL String
           -> String           -- ^ Variable name to define as inlined SQL
           -> Q [Dec]          -- ^ Result declarations
inlineSQL_ check sql declName = do
  check sql
  simpleValD (varName $ varCamelcaseName declName)
    [t| ShortByteString |]
    [| toShort $ T.encodeUtf8 $ T.pack $(stringE sql) |]
    -- IsString instance of ShortByteString type does not handle multi-byte characters.

inlineQuery_ :: (String -> Q ()) -- ^ action to check SQL string. for example to call prepare. if you do not need this, pass (const $ pure ())
             -> Query p a        -- ^ query to inline
             -> String           -- ^ Variable name to define as inlined query
             -> Q [Dec]          -- ^ Result declarations
inlineQuery_ check q declName = inlineSQL_ check (untypeQuery q) declName

inlineNoFetch_ :: UntypeableNoFetch s
              => (String -> Q ()) -- ^ action to check SQL string. for example to call prepare. if you do not need this, pass (const $ pure ())
              -> s a              -- ^ statement to inline
              -> String           -- ^ Variable name to define as inlined query
              -> Q [Dec]          -- ^ Result declarations
inlineNoFetch_ check q declName = inlineSQL_ check (untypeNoFetch q) declName

inlineUpdate_ :: (String -> Q ()) -- ^ action to check SQL string. for example to call prepare. if you do not need this, pass (const $ pure ())
              -> Update p         -- ^ statement to inline
              -> String           -- ^ Variable name to define as inlined query
              -> Q [Dec]          -- ^ Result declarations
inlineUpdate_ = inlineNoFetch_

inlineInsertValue_ :: (String -> Q ()) -- ^ action to check SQL string. for example to call prepare. if you do not need this, pass (const $ pure ())
                   -> Insert p         -- ^ statement to inline
                   -> String           -- ^ Variable name to define as inlined query
                   -> Q [Dec]          -- ^ Result declarations
inlineInsertValue_ = inlineNoFetch_

inlineInsertQuery_ :: (String -> Q ()) -- ^ action to check SQL string. for example to call prepare. if you do not need this, pass (const $ pure ())
                   -> InsertQuery p    -- ^ statement to inline
                   -> String           -- ^ Variable name to define as inlined query
                   -> Q [Dec]          -- ^ Result declarations
inlineInsertQuery_ = inlineNoFetch_

inlineDelete_ :: (String -> Q ()) -- ^ action to check SQL string. for example to call prepare. if you do not need this, pass (const $ pure ())
              -> Delete p         -- ^ statement to inline
              -> String           -- ^ Variable name to define as inlined query
              -> Q [Dec]          -- ^ Result declarations
inlineDelete_ = inlineNoFetch_

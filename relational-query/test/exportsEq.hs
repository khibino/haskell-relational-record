{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck.Simple (Test, eqTest, defaultMain)

import Export (onX, assignX, registerX, insertQueryX, deleteX)

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Short (ShortByteString, fromShort)

import Database.Relational (relationalQuery)
import Database.Relational.Export
  (inlineQuery_, inlineUpdate_, inlineInsertValue_, inlineInsertQuery_, inlineDelete_)


$(inlineQuery_
  (const $ return ())
  (relationalQuery onX)
  "inlineOnX")

$(inlineUpdate_
  (const $ return ())
  assignX
  "inlineAssignX")

$(inlineInsertValue_
  (const $ return ())
  registerX
  "inlineRegisterX")

$(inlineInsertQuery_
  (const $ return ())
  insertQueryX
  "inlineInsertQueryX")

$(inlineDelete_
  (const $ return ())
  deleteX
  "inlineDeleteX")

eqInline :: Show a => String -> ShortByteString -> a -> Test
eqInline name inline orig = eqTest name (B.unpack $ fromShort inline) (show orig)

tests :: [Test]
tests =
  [ eqInline "onX"          inlineOnX          onX
  , eqInline "assignX"      inlineAssignX      assignX
  , eqInline "registerX"    inlineRegisterX    registerX
  , eqInline "insertQueryX" inlineInsertQueryX insertQueryX
  , eqInline "deleteX"      inlineDeleteX      deleteX
  ]

main :: IO ()
main = defaultMain tests

module Export where

import Data.Functor.ProductIsomorphic (pureP, (|$|), (|*|))
import Data.Int (Int32)

import Model

import Database.Relational


onX :: Relation () (Maybe SetA, SetB)
onX = relation $ do
  a <- queryMaybe setA
  b <- query      setB
  on $ a ?! intA0' .=. just (b ! intB0')
  return $ (,) |$| a |*| b

assignX :: Update ()
assignX = update $ \_proj -> do
  intA0' <-# value (0 :: Int32)
  return $ pureP ()

registerX :: Insert (String, Maybe String)
registerX = insertValue $ do
  intC0' <-# value 1
  (ph1, ()) <- placeholder (\ph' -> strC1' <-# ph')
  intC2' <-# value 2
  (ph2, ()) <- placeholder (\ph' -> mayStrC3' <-# ph')
  return $ (,) |$| ph1 |*| ph2

setAFromB :: Pi SetB SetA
setAFromB =  SetA |$| intB0' |*| strB2' |*| strB2'

insertQueryX :: InsertQuery ()
insertQueryX =  insertQuery setAFromB setA

deleteX :: Delete ()
deleteX = delete $ \proj -> do
  wheres $ proj ! strA1' .=. value "A"
  return $ pureP ()

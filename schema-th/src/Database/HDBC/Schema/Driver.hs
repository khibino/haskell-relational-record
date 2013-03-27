-- |
-- Module      : Database.HDBC.Schema.Driver
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.HDBC.Schema.Driver (
  TypeMap,
  Driver(Driver, typeMap, getFieldsWithMap, getPrimaryKey),
  emptyDriver,
  getFields
  ) where

import Database.HDBC (IConnection)
import Language.Haskell.TH (TypeQ)

type TypeMap = [(String, TypeQ)]

data Driver conn =
  Driver
  { typeMap   :: TypeMap
  , getFieldsWithMap :: TypeMap
                        -> conn
                        -> String
                        -> String
                        -> IO ([(String, TypeQ)], [Int])
  , getPrimaryKey :: conn
                  -> String
                  -> String
                  -> IO (Maybe String)
  }

emptyDriver :: IConnection conn => Driver conn
emptyDriver =  Driver [] (\_ _ _ _ -> return ([],[])) (\_ _ _ -> return Nothing)

getFields :: IConnection conn => Driver conn -> conn -> String -> String -> IO ([(String, TypeQ)], [Int])
getFields drv = getFieldsWithMap drv (typeMap drv)

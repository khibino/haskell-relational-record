
module Database.HDBC.Schema.Driver (
  Driver(Driver, getFields, getPrimaryKey), emptyDriver
  ) where

import Database.HDBC (IConnection)
import Language.Haskell.TH (Q, Type)

data Driver conn =
  Driver
  { getFields :: conn
              -> String
              -> String
              -> IO ([(String, Q Type)], [Int])
  , getPrimaryKey :: conn
                  -> String
                  -> String
                  -> IO (Maybe String)
  }

emptyDriver :: IConnection conn => Driver conn
emptyDriver =  Driver (\_ _ _ -> return ([],[])) (\_ _ _ -> return Nothing)

module DS (definePgConTable) where

import Language.Haskell.TH
-- import Language.Haskell.TH.Name.CamelCase (ConName)
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Database.HDBC.Query.TH
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)

definePgConTable :: String -> String -> [Name] -> Q [Dec]
definePgConTable =
  defineTableFromDB (connectPostgreSQL "dbname=pgcon") driverPostgreSQL

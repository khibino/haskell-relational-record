
import Database.HDBC.MySQL
  (Connection, connectMySQL, defaultMySQLConnectInfo, MySQLConnectInfo (..))
import Test.Relational.QuickCheck.Tests
import Test.QuickCheck.Simple (defaultMain)

main =
  defaultMain . tests $
  connectMySQL
  defaultMySQLConnectInfo
  { mysqlUser = "hrr-tester", mysqlDatabase = "ARBITRARY0" }

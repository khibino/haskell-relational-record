
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Test.Relational.QuickCheck.Tests (tests)
import Test.QuickCheck.Simple (defaultMain)

main = defaultMain . tests $ connectPostgreSQL "dbname=hrrtest"

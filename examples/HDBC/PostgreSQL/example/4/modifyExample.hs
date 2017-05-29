{-# LANGUAGE FlexibleContexts #-}

import Prelude hiding (seq)
import Database.HDBC (IConnection, SqlValue, commit)
import Database.Record
import Database.Relational.Query
import Database.HDBC.Record
import Database.HDBC.Session
import qualified One
import StockGoods
import PgTestDataSource

import Data.Int (Int32)

stocks0 :: [StockGoods]
stocks0 =  [ StockGoods 1 "Apple"  110 40
           , StockGoods 2 "Orange" 150 30
           , StockGoods 3 "Banana" 90  15
           , StockGoods 4 "Cherry" 200 5
           ]

handleConnectionIO :: IConnection conn => IO conn -> (conn -> IO a) -> IO a
handleConnectionIO c p = handleSqlError' $ withConnectionIO c p

insertOne :: Insert Int32
insertOne = derivedInsert One.data'

runInsertList :: ToSql SqlValue a => Insert a -> [a] -> IO ()
runInsertList q ss = handleConnectionIO connect $ \conn -> do
  putStrLn $ "SQL: " ++ show q
  rvs  <- mapInsert conn q ss
  print rvs
  commit conn

runInsertOne0 :: IO ()
runInsertOne0 = runInsertList insertOne [1,2]

runInsertStocks0 :: IO ()
runInsertStocks0 =  runInsertList insertStockGoods stocks0


pine :: Relation () StockGoods
pine =  relation .
  return $ StockGoods |$| value 6 |*| value "Pine" |*| value 300 |*| value 3

insertPine :: InsertQuery ()
insertPine =  derivedInsertQuery id' pine

insertFig :: Insert ()
insertFig = derivedInsertValue $ do
  StockGoods.seq'    <-#  value 7
  StockGoods.name'   <-#  value "Fig"
  StockGoods.unit'   <-#  value 200
  StockGoods.amount' <-#  value 13
  return unitPH

runInsertQuery1 :: InsertQuery () -> IO ()
runInsertQuery1 ins = handleConnectionIO connect $ \conn -> do
  _ <- runInsertQuery conn ins ()
  commit conn

riseOfBanana :: Update ()
riseOfBanana =  derivedUpdate $ \proj -> do
  unit' <-# proj ! unit' .*. value 2
  wheres $ proj ! name' .=. value "Banana"
  return unitPH


newCherry :: StockGoods
newCherry =  StockGoods 5 "Black Cherry" 190 50

updateCherry :: Update (StockGoods, (Int32, String))
updateCherry =  derivedUpdateAllColumn $ \proj -> do
  (ph', ()) <- placeholder (\ph -> wheres $ proj ! (seq' >< name') .=. ph)
  return ph'

runUpdateAndPrint :: ToSql SqlValue p => Update p -> p -> IO ()
runUpdateAndPrint u p = handleConnectionIO connect $ \conn -> do
  putStrLn $ "SQL: " ++ show u
  rv <- runUpdate conn u p
  print rv
  commit conn

newOrange :: StockGoods
newOrange =  StockGoods 2 "Orange" 150 10

keyUpdateUidName :: KeyUpdate (Int32, String) StockGoods
keyUpdateUidName =  derivedKeyUpdate $ seq' >< name'

runKeyUpdateAndPrint :: ToSql SqlValue a => KeyUpdate p a -> a -> IO ()
runKeyUpdateAndPrint ku r = handleConnectionIO connect $ \conn -> do
  putStrLn $ "SQL: " ++ show ku
  rv <- runKeyUpdate conn ku r
  print rv
  commit conn

allStock :: IO [StockGoods]
allStock =  handleConnectionIO connect $ \conn -> do
  let q = stockGoods
  putStrLn $ "SQL: " ++ show q
  runQuery' conn (relationalQuery q) ()

deleteStock :: Delete Int32
deleteStock =  derivedDelete $ \proj -> do
  (ph', ()) <- placeholder (\ph -> wheres $ proj ! seq' .=. ph)
  return ph'

runDeleteStocks :: ToSql SqlValue a => Delete a -> [a] -> IO ()
runDeleteStocks d xs = handleConnectionIO connect $ \conn -> do
  putStrLn $ "SQL: " ++ show d
  ps  <- prepareDelete conn d
  rvs <- mapM (runPreparedDelete ps) xs
  print rvs
  commit conn

run :: IO ()
run = do
  runInsertOne0
  runInsertStocks0
  runInsertQuery1 insertPine
  runInsertList insertFig [()]
  runUpdateAndPrint riseOfBanana ()
  runUpdateAndPrint updateCherry (newCherry, (4, "Cherry"))
  runKeyUpdateAndPrint keyUpdateUidName newOrange
  ss <- allStock
  mapM_ print ss
  runDeleteStocks deleteStock (map seq ss)

main :: IO ()
main =  run

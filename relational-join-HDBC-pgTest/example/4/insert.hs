import qualified Database.HDBC  as HDBC
import Database.HDBC.Record.Insert
import SetA
import PgTestDataSource

run :: [SetA] -> IO ()
run rs = do
  conn <- connect
  let q =  insertSetA
  putStrLn $ "SQL: " ++ show q
  ps   <- prepare conn q
  rvs  <- mapM (`runPreparedInsert` ps) rs
  print rvs
  HDBC.commit conn
  HDBC.disconnect conn

recs :: [SetA]
recs =  [ SetA 1 "Apple"
        , SetA 2 "Orange"
        , SetA 5 "Banana"
        , SetA 6 "Cherry"
        ]

main :: IO ()
main =  run recs

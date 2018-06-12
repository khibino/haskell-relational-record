
import Test.QuickCheck.Simple (Test, defaultMain)
import qualified Test.QuickCheck.Simple as QSimple

import Lex (eqProp, eqProp')
import Model

import Control.Applicative ((<$>), (<*>))
import Data.Functor.ProductIsomorphic ((|$|), (|*|))
import Data.Int (Int32, Int64)

import Database.Relational


tables :: [Test]
tables =
  [ eqProp "setA" setA "SELECT int_a0, str_a1, str_a2 FROM TEST.set_a"
  , eqProp "setB" setB "SELECT int_b0, may_str_b1, str_b2 FROM TEST.set_b"
  , eqProp "setC" setC "SELECT int_c0, str_c1, int_c2, may_str_c3 FROM TEST.set_c"
  ]

_p_tables :: IO ()
_p_tables =  mapM_ print [show setA, show setB, show setC]


-- Monadic Operators tests

queryX :: Relation () SetA
queryX = relation $ do
  a <- query setA
  return a

queryMaybeX :: Relation () (Maybe SetA)
queryMaybeX = relation $ do
  a <- queryMaybe setA
  return a

onX :: Relation () (Maybe SetA, SetB)
onX = relation $ do
  a <- queryMaybe setA
  b <- query      setB
  on $ a ?! intA0' .=. just (b ! intB0')
  return $ (,) |$| a |*| b

wheresX :: Relation () (SetA, SetB)
wheresX = relation $ do
  a <- query      setA
  b <- query      setB
  wheres $ b ! intB0' .>=. value 3
  return $ (,) |$| a |*| b

groupByX :: Relation () (Int32, Integer)
groupByX = aggregateRelation $ do
  a <- query      setA
  ga0 <- groupBy $ a ! intA0'
  return $ (,) |$| ga0 |*| count (a ! intA0')

havingX :: Relation () Int
havingX = aggregateRelation $ do
  a <- query      setA
  let c = count (a ! intA0')
  having $ c .>. value 1
  return c

distinctX :: Relation () Int32
distinctX = relation $ do
  distinct
  a <- query      setA
  return $ a ! intA0'

all'X :: Relation () Int32
all'X = relation $ do
  all'
  a <- query      setA
  return $ a ! intA0'

assignX :: Update ()
assignX = update $ \_proj -> do
  intA0' <-# value (0 :: Int32)
  return unitPlaceHolder

registerX :: Insert (String, Maybe String)
registerX = insertValue $ do
  intC0' <-# value 1
  (ph1, ()) <- placeholder (\ph' -> strC1' <-# ph')
  intC2' <-# value 2
  (ph2, ()) <- placeholder (\ph' -> mayStrC3' <-# ph')
  return $ ph1 >< ph2

eqChunkedInsert :: String
                -> Insert a
                -> String
                -> String
                -> Test
eqChunkedInsert name ins prefix row =
    maybe
    (name, success)
    (\(sql, n) ->
      let estimate =
            unwords
            $ prefix
            : replicate (n - 1) (row ++ ",") ++ [row]
      in eqProp' name id sql estimate)
    $ chunkedInsert ins
  where
    success = QSimple.Bool Nothing True

monadic :: [Test]
monadic =
  [ eqProp "query"      queryX
    "SELECT ALL T0.int_a0 AS f0, T0.str_a1 AS f1, T0.str_a2 AS f2 FROM TEST.set_a T0"
  , eqProp "queryMaybe" queryMaybeX
    "SELECT ALL T0.int_a0 AS f0, T0.str_a1 AS f1, T0.str_a2 AS f2 FROM TEST.set_a T0"
  , eqProp "on"         onX
    "SELECT ALL T0.int_a0 AS f0, T0.str_a1 AS f1, T0.str_a2 AS f2, \
    \           T1.int_b0 AS f3, T1.may_str_b1 AS f4, T1.str_b2 AS f5 \
    \  FROM TEST.set_a T0 RIGHT JOIN TEST.set_b T1 ON (T0.int_a0 = T1.int_b0)"
  , eqProp "wheres"     wheresX
    "SELECT ALL T0.int_a0 AS f0, T0.str_a1 AS f1, T0.str_a2 AS f2, \
    \           T1.int_b0 AS f3, T1.may_str_b1 AS f4, T1.str_b2 AS f5 \
    \  FROM TEST.set_a T0 INNER JOIN TEST.set_b T1 ON (0=0) \
    \ WHERE (T1.int_b0 >= 3)"
  , eqProp "groupBy"    groupByX
    "SELECT ALL T0.int_a0 AS f0, COUNT(T0.int_a0) AS f1 \
    \  FROM TEST.set_a T0 GROUP BY T0.int_a0"
  , eqProp "having"     havingX
    "SELECT ALL COUNT(T0.int_a0) AS f0 FROM TEST.set_a T0 HAVING (COUNT(T0.int_a0) > 1)"
  , eqProp "distinct"   distinctX
    "SELECT DISTINCT T0.int_a0 AS f0 FROM TEST.set_a T0"
  , eqProp "all'"       all'X
    "SELECT ALL T0.int_a0 AS f0 FROM TEST.set_a T0"
  , eqProp "update"      assignX
    "UPDATE TEST.set_a SET int_a0 = 0"
  , eqProp "insert"      registerX
    "INSERT INTO TEST.set_c (int_c0, str_c1, int_c2, may_str_c3) VALUES (1, ?, 2, ?)"
  , eqChunkedInsert "insert chunked" registerX
    "INSERT INTO TEST.set_c (int_c0, str_c1, int_c2, may_str_c3) VALUES" "(1, ?, 2, ?)"
  ]

_p_monadic :: IO ()
_p_monadic =
  mapM_ putStrLn
  [ show queryX, show queryMaybeX, show onX, show wheresX
  , show groupByX, show havingX, show distinctX, show all'X
  , show assignX
  ]


-- Direct Join Operators

cross :: Relation () (SetA, SetB)
cross =  setA `inner` setB `on'` []

innerX :: Relation () (SetA, SetB)
innerX =  setA `inner` setB `on'` [ \a b -> a ! intA0' .=. b ! intB0' ]

leftX :: Relation () (SetA, Maybe SetB)
leftX =  setA `left` setB `on'` [ \a b -> just (a ! strA1') .=. b ?!? mayStrB1' ]

rightX :: Relation () (Maybe SetA, SetB)
rightX =  setA `right` setB  `on'` [ \a b -> a ?! intA0' .=. just (b ! intB0') ]

fullX :: Relation () (Maybe SetA, Maybe SetB)
fullX =  setA `full` setB `on'` [ \a b -> a ?! intA0' .=. b ?! intB0' ]

directJoins :: [Test]
directJoins =
  [ eqProp "cross" cross
    "SELECT ALL T0.int_a0 AS f0, T0.str_a1 AS f1, T0.str_a2 AS f2, \
    \           T1.int_b0 AS f3, T1.may_str_b1 AS f4, T1.str_b2 AS f5 \
    \  FROM TEST.set_a T0 INNER JOIN TEST.set_b T1 ON (0=0)"
  , eqProp "inner" innerX
    "SELECT ALL T0.int_a0 AS f0, T0.str_a1 AS f1, T0.str_a2 AS f2, \
    \           T1.int_b0 AS f3, T1.may_str_b1 AS f4, T1.str_b2 AS f5 \
    \  FROM TEST.set_a T0 INNER JOIN TEST.set_b T1 ON (T0.int_a0 = T1.int_b0)"
  , eqProp "left" leftX
    "SELECT ALL T0.int_a0 AS f0, T0.str_a1 AS f1, T0.str_a2 AS f2, \
    \           T1.int_b0 AS f3, T1.may_str_b1 AS f4, T1.str_b2 AS f5 \
    \  FROM TEST.set_a T0 LEFT JOIN TEST.set_b T1 ON (T0.str_a1 = T1.may_str_b1)"
  , eqProp "right" rightX
    "SELECT ALL T0.int_a0 AS f0, T0.str_a1 AS f1, T0.str_a2 AS f2, \
    \           T1.int_b0 AS f3, T1.may_str_b1 AS f4, T1.str_b2 AS f5 \
    \  FROM TEST.set_a T0 RIGHT JOIN TEST.set_b T1 ON (T0.int_a0 = T1.int_b0)"
  , eqProp "full" fullX
    "SELECT ALL T0.int_a0 AS f0, T0.str_a1 AS f1, T0.str_a2 AS f2, \
    \           T1.int_b0 AS f3, T1.may_str_b1 AS f4, T1.str_b2 AS f5 \
    \  FROM TEST.set_a T0 FULL JOIN TEST.set_b T1 ON (T0.int_a0 = T1.int_b0)"
  ]

_p_directJoins :: IO ()
_p_directJoins =  mapM_ print [show cross, show innerX, show leftX, show rightX, show fullX]


j3left :: Relation () Abc
j3left =  relation $ do
  a <- query setA
  b <- queryMaybe setB
  on $ just (a ! strA2') .=. b ?! strB2'
  c <- queryMaybe setC
  on $ b ?! intB0' .=. c ?! intC0'

  return $ Abc |$| a |*| b |*| c

j3right :: Relation () Abc
j3right =  relation $ do
  a  <- query setA
  bc <- query $ setB `full` setC `on'` [ \b c -> b ?! intB0' .=. c ?! intC0' ]
  let b = bc ! fst'
      c = bc ! snd'
  on $ just (a ! strA2') .=. b ?! strB2'

  return $ Abc |$| a |*| b |*| c

join3s :: [Test]
join3s =
  [ eqProp "join-3 left" j3left
    "SELECT ALL T0.int_a0 AS f0, T0.str_a1 AS f1, T0.str_a2 AS f2, \
    \           T1.int_b0 AS f3, T1.may_str_b1 AS f4, T1.str_b2 AS f5, \
    \           T2.int_c0 AS f6, T2.str_c1 AS f7, T2.int_c2 AS f8, T2.may_str_c3 AS f9 \
    \  FROM (TEST.set_a T0 LEFT JOIN TEST.set_b T1 ON (T0.str_a2 = T1.str_b2)) \
    \        LEFT JOIN TEST.set_c T2 ON (T1.int_b0 = T2.int_c0)"

  , eqProp "join-3 right" j3right
    "SELECT ALL T0.int_a0 AS f0, T0.str_a1 AS f1, T0.str_a2 AS f2, \
    \           T3.f0 AS f3, T3.f1 AS f4, T3.f2 AS f5, T3.f3 AS f6, T3.f4 AS f7, T3.f5 AS f8, T3.f6 AS f9 \
    \  FROM TEST.set_a T0 \
    \       INNER JOIN (SELECT ALL T1.int_b0 AS f0, T1.may_str_b1 AS f1, T1.str_b2 AS f2, \
    \                              T2.int_c0 AS f3, T2.str_c1 AS f4, T2.int_c2 AS f5, T2.may_str_c3 AS f6 \
    \                     FROM TEST.set_b T1 FULL JOIN TEST.set_c T2 ON (T1.int_b0 = T2.int_c0)) T3 \
    \               ON (T0.str_a2 = T3.f2)"
  ]

_p_j3s :: IO ()
_p_j3s =  mapM_ print [show j3left, show j3right]


-- Index of Nested Projections

nestedPiRec :: Relation () SetA
nestedPiRec = relation $ do
  ar <- query . relation $ do
    a <- query setA
    return $ value "Hello" >< a
  return $ ar ! snd'

nestedPiCol :: Relation () String
nestedPiCol = relation $ do
  ar <- query . relation $ do
    a <- query setA
    return $ a >< value "Hello"
  return $ ar ! snd'

nestedPi :: Relation () String
nestedPi = relation $ do
  ar <- query . relation $ do
    a <- query setA
    return $ (value "Hello" >< a) >< value "World"
  return $ ar ! snd'

nested :: [Test]
nested =
  [ eqProp "nested pi record" nestedPiRec
    "SELECT ALL T1.f1 AS f0, T1.f2 AS f1, T1.f3 AS f2 \
    \  FROM (SELECT ALL 'Hello' AS f0, \
    \                   T0.int_a0 AS f1, T0.str_a1 AS f2, T0.str_a2 AS f3 \
    \              FROM TEST.set_a T0) T1"

  , eqProp "nested pi column" nestedPiCol
    "SELECT ALL T1.f3 AS f0 \
    \      FROM (SELECT ALL T0.int_a0 AS f0, T0.str_a1 AS f1, T0.str_a2 AS f2, \
    \                       'Hello' AS f3 \
    \                  FROM TEST.set_a T0) T1"

  , eqProp "nested pi both" nestedPi
    "SELECT ALL T1.f4 AS f0 \
    \      FROM (SELECT ALL 'Hello' AS f0, \
    \                       T0.int_a0 AS f1, T0.str_a1 AS f2, T0.str_a2 AS f3, \
    \                       'World' AS f4 \
    \                  FROM TEST.set_a T0) T1"
  ]

_p_nested :: IO ()
_p_nested =  mapM_ print [show nestedPiRec, show nestedPiCol, show nestedPi]


-- Record Operators

bin53 :: (Record Flat Int32 -> Record Flat Int32 -> Record Flat r) -> Relation () r
bin53 op = relation $ do
  return $ value 5 `op` value 3

strIn :: Relation () (Maybe Bool)
strIn = relation $
  return $ value "foo" `in'` values ["foo", "bar"]

boolTF :: (Record Flat (Maybe Bool) -> Record Flat (Maybe Bool) -> Record Flat r) -> Relation () r
boolTF op = relation $ do
  return $ valueTrue `op` valueFalse

strConcat :: Relation () String
strConcat = relation $ do
  return $ value "Hello, " .||. value "World!"

strLike :: Relation () (Maybe Bool)
strLike = relation $ do
  return $ value "Hoge" `like` "H%"

_p_bin53 :: (Record Flat Int32 -> Record Flat Int32 -> Record Flat r) -> IO ()
_p_bin53 = print . bin53

bin :: [Test]
bin =
  [ eqProp "equal" (bin53 (.=.))  "SELECT ALL (5 =  3) AS f0"
  , eqProp "lt"    (bin53 (.<.))  "SELECT ALL (5 <  3) AS f0"
  , eqProp "le"    (bin53 (.<=.)) "SELECT ALL (5 <= 3) AS f0"
  , eqProp "gt"    (bin53 (.>.))  "SELECT ALL (5 >  3) AS f0"
  , eqProp "ge"    (bin53 (.>=.)) "SELECT ALL (5 >= 3) AS f0"
  , eqProp "ne"    (bin53 (.<>.)) "SELECT ALL (5 <> 3) AS f0"

  , eqProp "and"   (boolTF and')  "SELECT ALL ((0=0) AND (0=1)) AS f0"
  , eqProp "or"    (boolTF or')   "SELECT ALL ((0=0) OR  (0=1)) AS f0"

  , eqProp "in"    strIn          "SELECT ALL ('foo' IN ('foo', 'bar')) AS f0"

  , eqProp "string concat" strConcat "SELECT ALL ('Hello, ' || 'World!') AS f0"
  , eqProp "like" strLike "SELECT ALL ('Hoge' LIKE 'H%') AS f0"

  , eqProp "plus"  (bin53 (.+.)) "SELECT ALL (5 + 3) AS f0"
  , eqProp "minus" (bin53 (.-.)) "SELECT ALL (5 - 3) AS f0"
  , eqProp "mult"  (bin53 (.*.)) "SELECT ALL (5 * 3) AS f0"
  , eqProp "div"   (bin53 (./.)) "SELECT ALL (5 / 3) AS f0"
  ]

caseSearchX :: Relation () String
caseSearchX = relation $ do
  return $
    caseSearch
    [ (value 2 .=. value (1  :: Int32)            , value "foo")
    , (value 5 .=. value 3 .+. value (2 :: Int32) , value "bar")
    , (value "a" .=. value "b"                    , value "baz") ]
    (value "other")

caseX :: Relation () String
caseX = relation $ do
  return $
    case'
    (value (5 :: Int32))
    [ (value 1             , value "foo")
    , (value 3 .+. value 2 , value "bar")
    , (value 10            , value "baz") ]
    (value "other")

caseRecordX :: Relation () Int32
caseRecordX = relation $ do
  return $
    case'
    (value (5 :: Int32))
    [ (value 1             , (,) |$| value 1 |*| value "foo")
    , (value 3 .+. value 2 , (,) |$| value 2 |*| value "bar")
    , (value 10            , (,) |$| value 3 |*| value "baz") ]
    ((,) |$| value (0 :: Int32) |*| value "other")
    ! fst'
    .*.
    value 10

caseRecordMaybeX :: Relation () (Maybe (Int32, String))
caseRecordMaybeX = relation $ do
  return $
    caseMaybe
    (value (5 :: Int32))
    [ (value (1 :: Int32)  , just $ (,) |$| value (1 :: Int32) |*| value "foo")
    , (value 3 .+. value 2 , just $ (,) |$| value 2            |*| value "bar") ]

cases :: [Test]
cases =
  [ eqProp "caseSearch" caseSearchX
    "SELECT ALL CASE WHEN (2 = 1) THEN 'foo' WHEN (5 = (3 + 2)) THEN 'bar' WHEN ('a' = 'b') THEN 'baz' ELSE 'other' END AS f0"
  , eqProp "case"       caseX
    "SELECT ALL CASE 5 WHEN 1 THEN 'foo' WHEN (3 + 2) THEN 'bar' WHEN 10 THEN 'baz' ELSE 'other' END AS f0"
  , eqProp "caseRecord" caseRecordX
    "SELECT ALL (CASE 5 WHEN 1 THEN 1 WHEN (3 + 2) THEN 2 WHEN 10 THEN 3 ELSE 0 END * 10) AS f0"
  , eqProp "caseRecordMaybe" caseRecordMaybeX
    "SELECT ALL CASE 5 WHEN 1 THEN 1 WHEN (3 + 2) THEN 2 ELSE NULL END AS f0, \
              \ CASE 5 WHEN 1 THEN 'foo' WHEN (3 + 2) THEN 'bar' ELSE NULL END AS f1"
  ]

_p_cases :: IO ()
_p_cases =
  mapM_ print [show caseSearchX, show caseX]

nothingX :: Relation () (SetA, Maybe SetB)
nothingX =  relation $ do
  a <- query setA
  b <- queryMaybe setB

  wheres $ isNothing b `or'` a ! intA0' .=. value 1

  return $ a >< b

justX :: Relation () (SetA, Maybe SetB)
justX =  relation $ do
  a <- query setA
  b <- queryMaybe setB

  wheres $ isJust b `or'` a ! intA0' .=. value 1

  return $ a >< b

maybeX :: Relation () (Int32, SetB)
maybeX =  relation $ do
  a <- queryMaybe setA
  b <- query setB

  wheres $ a ?! strA2' .=. b ! mayStrB1'

  return $ fromMaybe (value 1) (a ?! intA0') >< b

notX :: Relation () (Maybe Bool)
notX = relation $
  return $ not' valueFalse

existsX :: Relation () (Maybe Bool)
existsX = relation $
  return . exists =<< queryList setA

uni :: [Test]
uni =
  [ eqProp "isNothing" nothingX
    "SELECT ALL T0.int_a0 AS f0, T0.str_a1 AS f1, T0.str_a2 AS f2, \
    \           T1.int_b0 AS f3, T1.may_str_b1 AS f4, T1.str_b2 AS f5 \
    \  FROM TEST.set_a T0 LEFT JOIN TEST.set_b T1 ON (0=0) \
    \ WHERE ((T1.int_b0 IS NULL) OR (T0.int_a0 = 1))"
  , eqProp "isJust" justX
    "SELECT ALL T0.int_a0 AS f0, T0.str_a1 AS f1, T0.str_a2 AS f2, \
    \           T1.int_b0 AS f3, T1.may_str_b1 AS f4, T1.str_b2 AS f5 \
    \  FROM TEST.set_a T0 LEFT JOIN TEST.set_b T1 ON (0=0) \
    \ WHERE ((NOT (T1.int_b0 IS NULL)) OR (T0.int_a0 = 1))"
  , eqProp "fromMaybe" maybeX
    "SELECT ALL CASE WHEN (T0.int_a0 IS NULL) THEN 1 ELSE T0.int_a0 END AS f0, \
    \           T1.int_b0 AS f1, T1.may_str_b1 AS f2, T1.str_b2 AS f3 \
    \  FROM TEST.set_a T0 RIGHT JOIN TEST.set_b T1 ON (0=0) WHERE (T0.str_a2 = T1.may_str_b1)"
  , eqProp "not" notX
    "SELECT ALL (NOT (0=1)) AS f0"
  , eqProp "exists" existsX
    "SELECT ALL (EXISTS (SELECT int_a0, str_a1, str_a2 FROM TEST.set_a)) AS f0"
  ]

_p_uni :: IO ()
_p_uni =  mapM_ print [show nothingX, show justX, show maybeX, show notX, show existsX]

groupX :: Relation () (String, Int64)
groupX =  aggregateRelation $ do
  c <- query setC

  gc1 <- groupBy $ c ! strC1'
  return $ gc1 >< count (c ! intC0')

cubeX :: Relation () ((Maybe String, Maybe (Int64, Maybe String)), Maybe Int32)
cubeX =  aggregateRelation $ do
  c <- query setC

  gCube <- groupBy' . cube $ do
    (><)
      <$> bkey (c ! strC1')
      <*> bkey (c ! intC2' >< c ! mayStrC3')
  return $ gCube >< sum' (c ! intC0')

groupingSetsX :: Relation () (((Maybe String, Maybe (Maybe String)), Maybe Int64), Maybe Int64)
groupingSetsX = aggregateRelation $ do
  c <- query setC

  gs <- groupBy' . groupingSets $ do
    s1 <- set $ do
      gRollup <- key' . rollup $ do
        (><)
          <$> bkey (c ! strC1')
          <*> bkey (c ! mayStrC3')
      gc2 <- key $ c ! intC2'
      return $ gRollup >< gc2
    s2 <- set . key $ c ! intC2'
    return $ s1 >< s2

  return gs


groups :: [Test]
groups =
  [ eqProp "group" groupX
    "SELECT ALL T0.str_c1 AS f0, COUNT(T0.int_c0) AS f1 \
    \  FROM TEST.set_c T0 GROUP BY T0.str_c1"
  , eqProp "cube" cubeX
    "SELECT ALL T0.str_c1 AS f0, T0.int_c2 AS f1, T0.may_str_c3 AS f2, SUM(T0.int_c0) AS f3 \
    \  FROM TEST.set_c T0 GROUP BY CUBE ((T0.str_c1), (T0.int_c2, T0.may_str_c3))"
  , eqProp "groupingSets" groupingSetsX
    "SELECT ALL T0.str_c1 AS f0, T0.may_str_c3 AS f1, T0.int_c2 AS f2, T0.int_c2 AS f3 \
    \  FROM TEST.set_c T0 GROUP BY \
    \  GROUPING SETS ((ROLLUP ((T0.str_c1), (T0.may_str_c3)), T0.int_c2), (T0.int_c2))"
  ]

_p_groups :: IO ()
_p_groups =  mapM_ print [show groupX, show cubeX, show groupingSetsX]

ordFlatX :: Relation () (SetA, Maybe SetB)
ordFlatX =  relation $ do
  a <- query setA
  b <- queryMaybe setB
  on $ just (a ! strA2') .=. b ?! strB2'

  orderBy (a ! strA1') Asc
  orderBy' (b ?! mayStrB1') Desc NullsLast

  return $ (,) |$| a |*| b

ordFlatY :: Relation () (SetA, Maybe SetB)
ordFlatY =  relation $ do
  a <- query setA
  b <- queryMaybe setB
  on $ just (a ! strA2') .=. b ?! strB2'

  orderBy (a ! strA1') Asc
  orderBy' b Desc NullsLast

  return $ (,) |$| a |*| b

ordAggX :: Relation () (String, Int64)
ordAggX =  aggregateRelation $ do
  c <- query setC

  gc1 <- groupBy $ c ! strC1'

  orderBy' (sum' $ c ! intC0') Asc NullsFirst

  return $ gc1 >< count (c ! intC0')

_p_orders :: IO ()
_p_orders = mapM_ print [show ordFlatX, show ordAggX]

orders :: [Test]
orders =
  [ eqProp "order-by - flat" ordFlatX
    "SELECT ALL T0.int_a0 AS f0, T0.str_a1 AS f1, T0.str_a2 AS f2, \
    \           T1.int_b0 AS f3, T1.may_str_b1 AS f4, T1.str_b2 AS f5 \
    \  FROM TEST.set_a T0 LEFT JOIN TEST.set_b T1 ON (T0.str_a2 = T1.str_b2) \
    \  ORDER BY T0.str_a1 ASC, T1.may_str_b1 DESC NULLS LAST"
  , eqProp "order-by - flag 2" ordFlatY
    "SELECT ALL T0.int_a0 AS f0, T0.str_a1 AS f1, T0.str_a2 AS f2, \
    \           T1.int_b0 AS f3, T1.may_str_b1 AS f4, T1.str_b2 AS f5 \
    \  FROM TEST.set_a T0 LEFT JOIN TEST.set_b T1 ON (T0.str_a2 = T1.str_b2) \
    \  ORDER BY T0.str_a1 ASC, T1.int_b0 DESC NULLS LAST, \
    \           T1.may_str_b1 DESC NULLS LAST, T1.str_b2 DESC NULLS LAST"
  , eqProp "order-by - aggregated" ordAggX
    "SELECT ALL T0.str_c1 AS f0, COUNT(T0.int_c0) AS f1 \
    \  FROM TEST.set_c T0 GROUP BY T0.str_c1 ORDER BY SUM(T0.int_c0) ASC NULLS FIRST"
  ]

partitionX :: Relation () (String, Int64)
partitionX =  relation $ do
  c <- query setC

  return $ (c ! strC1') >< rank `over` do
    partitionBy $ c ! strC1'
    orderBy (c ! intC2') Asc

partitionY :: Relation () (String, (Int64, Maybe Int32))
partitionY =  relation $ do
  c <- query setC

  return $ (c ! strC1') >< (rank >< sum' (c ! intC0'))`over` do
    partitionBy $ c ! strC1'
    orderBy (c ! intC2') Asc

partitions :: [Test]
partitions =
  [ eqProp "partition 0"  partitionX
    "SELECT ALL T0.str_c1 AS f0, \
    \           RANK() OVER (PARTITION BY T0.str_c1 ORDER BY T0.int_c2 ASC) AS f1 \
    \  FROM TEST.set_c T0"
  , eqProp "partition 1"  partitionY
    "SELECT ALL T0.str_c1 AS f0, \
    \           RANK()         OVER (PARTITION BY T0.str_c1 ORDER BY T0.int_c2 ASC) AS f1, \
    \           SUM(T0.int_c0) OVER (PARTITION BY T0.str_c1 ORDER BY T0.int_c2 ASC) AS f2 \
    \      FROM TEST.set_c T0"
  ]

_p_partitions :: IO ()
_p_partitions =  mapM_ print [show partitionX, show partitionY]

setAFromB :: Pi SetB SetA
setAFromB =  SetA |$| intB0' |*| strB2' |*| strB2'

aFromB :: Relation () SetA
aFromB =  relation $ do
  x <- query setB
  return $ x ! setAFromB

unionX :: Relation () SetA
unionX =  setA `union` aFromB

unionAllX :: Relation () SetA
unionAllX =  setA `unionAll` aFromB

exceptX :: Relation () SetA
exceptX =  setA `except` aFromB

intersectX :: Relation () SetA
intersectX =  setA `intersect` aFromB

exps :: [Test]
exps =
  [ eqProp "union" unionX
    "SELECT int_a0 AS f0, str_a1 AS f1, str_a2 AS f2 FROM TEST.set_a UNION \
    \SELECT ALL T0.int_b0 AS f0, T0.str_b2 AS f1, T0.str_b2 AS f2 FROM TEST.set_b T0"
  , eqProp "unionAll" unionAllX
    "SELECT int_a0 AS f0, str_a1 AS f1, str_a2 AS f2 FROM TEST.set_a UNION ALL \
    \SELECT ALL T0.int_b0 AS f0, T0.str_b2 AS f1, T0.str_b2 AS f2 FROM TEST.set_b T0"
  , eqProp "except" exceptX
    "SELECT int_a0 AS f0, str_a1 AS f1, str_a2 AS f2 FROM TEST.set_a EXCEPT \
    \SELECT ALL T0.int_b0 AS f0, T0.str_b2 AS f1, T0.str_b2 AS f2 FROM TEST.set_b T0"
  , eqProp "intersect" intersectX
    "SELECT int_a0 AS f0, str_a1 AS f1, str_a2 AS f2 FROM TEST.set_a INTERSECT \
    \SELECT ALL T0.int_b0 AS f0, T0.str_b2 AS f1, T0.str_b2 AS f2 FROM TEST.set_b T0"
  ]

insertX :: Insert SetA
insertX =  insert id'

insertI :: Insert SetI
insertI =  insert id'

insertQueryX :: InsertQuery ()
insertQueryX =  insertQuery setAFromB setA

updateKeyX :: KeyUpdate Int32 SetA
updateKeyX =  primaryUpdate tableOfSetA

updateX :: Update ()
updateX =  update $ \proj -> do
  strA2' <-# value "X"
  wheres $ proj ! strA1' .=. value "A"
  return unitPlaceHolder

deleteX :: Delete ()
deleteX =  delete $ \proj -> do
  wheres $ proj ! strA1' .=. value "A"
  return unitPlaceHolder

effs :: [Test]
effs =
  [ eqProp "insert" insertX
    "INSERT INTO TEST.set_a (int_a0, str_a1, str_a2) VALUES (?, ?, ?)"
  , eqChunkedInsert "insert chunked" insertX
    "INSERT INTO TEST.set_a (int_a0, str_a1, str_a2) VALUES" "(?, ?, ?)"
  , eqProp "insert1" insertI
    "INSERT INTO TEST.set_i (int_i0) VALUES (?)"
  , eqChunkedInsert "insert1 chunked" insertI
    "INSERT INTO TEST.set_i (int_i0) VALUES" "(?)"
  , eqProp "insertQuery" insertQueryX
    "INSERT INTO TEST.set_b (int_b0, str_b2, str_b2) SELECT int_a0, str_a1, str_a2 FROM TEST.set_a"
  , eqProp "updateKey" updateKeyX
    "UPDATE TEST.set_a SET str_a1 = ?, str_a2 = ? WHERE int_a0 = ?"
  , eqProp "update" updateX
    "UPDATE TEST.set_a SET str_a2 = 'X' WHERE (str_a1 = 'A')"
  , eqProp "delete" deleteX
    "DELETE FROM TEST.set_a WHERE (str_a1 = 'A')"
  ]

updateExistsX :: Update ()
updateExistsX = update $ \proj -> do
  strA2' <-# value "X"
  wheres . exists
    =<< (queryList . relation $ do
            b <- query setB
            wheres $ b ! intB0' .=. proj ! intA0'
            return b)
  return unitPlaceHolder

updateScalarX :: Update ()
updateScalarX = update $ \proj -> do
  strA2' <-# value "X"
  sb <- queryScalar . unsafeUnique . relation $ do
    b <- query setB
    wheres $ b ! intB0' .=. value 0
    return $ b ! intB0'
  wheres $ just (proj ! intA0') .=. sb
  return unitPlaceHolder

deleteExistsX :: Delete ()
deleteExistsX =  delete $ \proj -> do
  wheres . exists
    =<< (queryList . relation $ do
            b <- query setB
            wheres $ b ! intB0' .=. proj ! intA0'
            return b)
  return unitPlaceHolder

deleteScalarX :: Delete ()
deleteScalarX = delete $ \proj -> do
  sb <- queryScalar . unsafeUnique . relation $ do
    b <- query setB
    wheres $ b ! intB0' .=. value 0
    return $ b ! intB0'
  wheres $ just (proj ! intA0') .=. sb
  return unitPlaceHolder

correlated :: [Test]
correlated =
  [ eqProp "update-exists" updateExistsX
    "UPDATE TEST.set_a SET str_a2 = 'X' \
    \ WHERE (EXISTS (SELECT ALL T0.int_b0 AS f0, T0.may_str_b1 AS f1, T0.str_b2 AS f2 \
    \                      FROM TEST.set_b T0 \
    \                     WHERE (T0.int_b0 = int_a0)))"

  , eqProp "update-scalar" updateScalarX
    "UPDATE TEST.set_a SET str_a2 = 'X' \
    \ WHERE (int_a0 = (SELECT ALL T0.int_b0 AS f0 \
    \                        FROM TEST.set_b T0 \
    \                       WHERE (T0.int_b0 = 0)))"

  , eqProp "delete-exists" deleteExistsX
   "DELETE FROM TEST.set_a \
   \ WHERE (EXISTS (SELECT ALL T0.int_b0 AS f0, T0.may_str_b1 AS f1, T0.str_b2 AS f2 \
   \                      FROM TEST.set_b T0 \
   \                     WHERE (T0.int_b0 = int_a0)))"

  , eqProp "delete-scalar" deleteScalarX
    "DELETE FROM TEST.set_a \
    \ WHERE (int_a0 = (SELECT ALL T0.int_b0 AS f0 \
    \                        FROM TEST.set_b T0 \
    \                       WHERE (T0.int_b0 = 0)))"
  ]

tests :: [Test]
tests =
  concat [ tables, monadic, directJoins, join3s, nested, bin, cases, uni
         , groups, orders, partitions, exps, effs, correlated]

main :: IO ()
main = defaultMain tests

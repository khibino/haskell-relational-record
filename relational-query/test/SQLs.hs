module SQLs (tests) where

import Distribution.TestSuite (Test)
import Distribution.TestSuite.Compat (TestList, testList)

import Control.Applicative ((<$>), (<*>))

import Lex (eqProp)
import Model

import Data.Int (Int32, Int64)
import Database.Relational.Query


numBin :: (Projection Flat Int32 -> Projection Flat Int32 -> Projection Flat r) -> Relation () r
numBin op = relation $ do
  return $ value 5 `op` value 3

_p_numBin :: (Projection Flat Int32 -> Projection Flat Int32 -> Projection Flat r) -> IO ()
_p_numBin = print . numBin

bin :: [Test]
bin =
  [ eqProp "plus"  (numBin (.+.)) "SELECT ALL (5 + 3) AS f0"
  , eqProp "minus" (numBin (.-.)) "SELECT ALL (5 - 3) AS f0"
  , eqProp "mult"  (numBin (.*.)) "SELECT ALL (5 * 3) AS f0"
  , eqProp "div"   (numBin (./.)) "SELECT ALL (5 / 3) AS f0"
  ]

tables :: [Test]
tables =
  [ eqProp "setA" setA "SELECT int_a0, str_a1, str_a2 FROM TEST.set_a"
  , eqProp "setB" setB "SELECT int_b0, may_str_b1, str_b2 FROM TEST.set_b"
  , eqProp "setC" setC "SELECT int_c0, str_c1, int_c2, may_str_c3 FROM TEST.set_c"
  ]

_p_tables :: IO ()
_p_tables =  mapM_ print [show setA, show setB, show setC]

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

maybes :: [Test]
maybes =
  [ eqProp "isJust" justX
    "SELECT ALL T0.int_a0 AS f0, T0.str_a1 AS f1, T0.str_a2 AS f2, \
    \           T1.int_b0 AS f3, T1.may_str_b1 AS f4, T1.str_b2 AS f5 \
    \  FROM TEST.set_a T0 LEFT JOIN TEST.set_b T1 ON (0=0) \
    \ WHERE ((NOT (T1.int_b0 IS NULL)) OR (T0.int_a0 = 1))"
  , eqProp "fromMaybe" maybeX
    "SELECT ALL CASE WHEN (T0.int_a0 IS NULL) THEN 1 ELSE T0.int_a0 END AS f0, \
    \           T1.int_b0 AS f1, T1.may_str_b1 AS f2, T1.str_b2 AS f3 \
    \  FROM TEST.set_a T0 RIGHT JOIN TEST.set_b T1 ON (0=0) WHERE (T0.str_a2 = T1.may_str_b1)"
  ]

_p_maybes :: IO ()
_p_maybes =  mapM_ print [show justX, show maybeX]

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
insertX =  derivedInsert id'

insertQueryX :: InsertQuery ()
insertQueryX =  derivedInsertQuery setAFromB setA

updateKeyX :: KeyUpdate Int32 SetA
updateKeyX =  primaryUpdate tableOfSetA

updateX :: Update ()
updateX =  derivedUpdate $ \proj -> do
  strA2' <-# value "X"
  wheres $ proj ! strA1' .=. value "A"
  return unitPlaceHolder

deleteX :: Delete ()
deleteX =  derivedDelete $ \proj -> do
  wheres $ proj ! strA1' .=. value "A"
  return unitPlaceHolder

effs :: [Test]
effs =
  [ eqProp "insert" insertX
    "INSERT INTO TEST.set_a (int_a0, str_a1, str_a2) VALUES (?, ?, ?)"
  , eqProp "insertQuery" insertQueryX
    "INSERT INTO TEST.set_b (int_b0, str_b2, str_b2) SELECT int_a0, str_a1, str_a2 FROM TEST.set_a"
  , eqProp "updateKey" updateKeyX
    "UPDATE TEST.set_a SET str_a1 = ?, str_a2 = ? WHERE int_a0 = ?"
  , eqProp "update" updateX
    "UPDATE TEST.set_a SET str_a2 = 'X' WHERE (str_a1 = 'A')"
  , eqProp "delete" deleteX
    "DELETE FROM TEST.set_a WHERE (str_a1 = 'A')"
  ]

tests :: TestList
tests =
  testList
  $ concat [bin, tables, directJoins, join3s, maybes,
            groups, partitions, exps, effs]

module SQLs (tests) where

import Distribution.TestSuite (Test)

import Tool (eqShow)
import Model

import Data.Int (Int32)
import Database.Relational.Query

base :: [Test]
base =
  [ eqShow "setA" setA "SELECT int_a0, str_a1, str_a2 FROM TEST.set_a"
  , eqShow "setB" setB "SELECT int_b0, may_str_b1, str_b2 FROM TEST.set_b"
  , eqShow "setC" setC "SELECT int_c0, str_c1, int_c2, may_str_c3 FROM TEST.set_c"
  ]

_p_base :: IO ()
_p_base =  mapM_ print [show setA, show setB, show setC]

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
  [ eqShow "cross" cross
    "SELECT ALL T0.int_a0 AS f0, T0.str_a1 AS f1, T0.str_a2 AS f2, T1.int_b0 AS f3, T1.may_str_b1 AS f4, T1.str_b2 AS f5 FROM TEST.set_a T0 INNER JOIN TEST.set_b T1 ON (0=0)"
  , eqShow "inner" innerX
    "SELECT ALL T0.int_a0 AS f0, T0.str_a1 AS f1, T0.str_a2 AS f2, T1.int_b0 AS f3, T1.may_str_b1 AS f4, T1.str_b2 AS f5 FROM TEST.set_a T0 INNER JOIN TEST.set_b T1 ON (T0.int_a0 = T1.int_b0)"
  , eqShow "left" leftX
    "SELECT ALL T0.int_a0 AS f0, T0.str_a1 AS f1, T0.str_a2 AS f2, T1.int_b0 AS f3, T1.may_str_b1 AS f4, T1.str_b2 AS f5 FROM TEST.set_a T0 LEFT JOIN TEST.set_b T1 ON (T0.str_a1 = T1.may_str_b1)"
  , eqShow "right" rightX
    "SELECT ALL T0.int_a0 AS f0, T0.str_a1 AS f1, T0.str_a2 AS f2, T1.int_b0 AS f3, T1.may_str_b1 AS f4, T1.str_b2 AS f5 FROM TEST.set_a T0 RIGHT JOIN TEST.set_b T1 ON (T0.int_a0 = T1.int_b0)"
  , eqShow "full" fullX
    "SELECT ALL T0.int_a0 AS f0, T0.str_a1 AS f1, T0.str_a2 AS f2, T1.int_b0 AS f3, T1.may_str_b1 AS f4, T1.str_b2 AS f5 FROM TEST.set_a T0 FULL JOIN TEST.set_b T1 ON (T0.int_a0 = T1.int_b0)"
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
join3s =  [ eqShow "join-3 left" j3left
            "SELECT ALL T0.int_a0 AS f0, T0.str_a1 AS f1, T0.str_a2 AS f2, T1.int_b0 AS f3, T1.may_str_b1 AS f4, T1.str_b2 AS f5, T2.int_c0 AS f6, T2.str_c1 AS f7, T2.int_c2 AS f8, T2.may_str_c3 AS f9 FROM (TEST.set_a T0 LEFT JOIN TEST.set_b T1 ON (T0.str_a2 = T1.str_b2)) LEFT JOIN TEST.set_c T2 ON (T1.int_b0 = T2.int_c0)"

          , eqShow "join-3 right" j3right
            "SELECT ALL T0.int_a0 AS f0, T0.str_a1 AS f1, T0.str_a2 AS f2, T3.f0 AS f3, T3.f1 AS f4, T3.f2 AS f5, T3.f3 AS f6, T3.f4 AS f7, T3.f5 AS f8, T3.f6 AS f9 FROM TEST.set_a T0 INNER JOIN (SELECT ALL T1.int_b0 AS f0, T1.may_str_b1 AS f1, T1.str_b2 AS f2, T2.int_c0 AS f3, T2.str_c1 AS f4, T2.int_c2 AS f5, T2.may_str_c3 AS f6 FROM TEST.set_b T1 FULL JOIN TEST.set_c T2 ON (T1.int_b0 = T2.int_c0)) T3 ON (T0.str_a2 = T3.f2)"
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
maybes =  [ eqShow "isJust" justX
            "SELECT ALL T0.int_a0 AS f0, T0.str_a1 AS f1, T0.str_a2 AS f2, T1.int_b0 AS f3, T1.may_str_b1 AS f4, T1.str_b2 AS f5 FROM TEST.set_a T0 LEFT JOIN TEST.set_b T1 ON (0=0) WHERE ((NOT (T1.int_b0 IS NULL)) OR (T0.int_a0 = 1))"
          , eqShow "fromMaybe" maybeX
            "SELECT ALL CASE WHEN (T0.int_a0 IS NULL) THEN 1 ELSE T0.int_a0 END AS f0, T1.int_b0 AS f1, T1.may_str_b1 AS f2, T1.str_b2 AS f3 FROM TEST.set_a T0 RIGHT JOIN TEST.set_b T1 ON (0=0) WHERE (T0.str_a2 = T1.may_str_b1)"
          ]

_p_maybes :: IO ()
_p_maybes =  mapM_ print [show justX, show maybeX]


tests :: IO [Test]
tests =
  return $ concat [base, directJoins, join3s, maybes]

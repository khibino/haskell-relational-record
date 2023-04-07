% Relational Record Released!
% 2014-12-20
% Kei Hibino

Basics
-----

Building join like List Comprehension or List Monad

$$\{ (x, y) | x \in X, y \in Y, \pi_1(x) = \pi_2(y) \}$$

~~~~~ {#mycode .haskell}
[ (x, y) | x <- xs, y <- ys, fst x == snd y ] -- Comprehension

do { x <- xs; y <- ys; guard (fst x == snd y); return (x, y) } -- List Monad

personAndBirthday :: Relation () (Person, Birthday)
personAndBirthday =  relation $ do
  p <- query person     -- Join product accumulated
  b <- query birthday
  wheres $ p ! Person.name' .=. b ! Birthday.name'
  return $ p >< b
~~~~~

Outer Join
-----

Outer joining. Not matched records is mapped Maybe type.

~~~~~ {#mycode .haskell}
personAndBirthdayL :: Relation () (Person, Maybe Birthday)
personAndBirthdayL =  relation $ do
  p <- query person
  b <- queryMaybe birthday  -- Maybe not match
  wheres $ just (p ! Person.name') .=. b ?! Birthday.name'
  return $ p >< b
~~~~~

Aggregation
-----

* groupBy action only used in aggregated context Monad.
* Only aggregated context Monad with aggregated context result type is allowed to pass aggregateRelation.

~~~~~ {#mycode .haskell}
agesOfFamilies :: Relation () (String, Maybe Int32)
agesOfFamilies =  aggregateRelation $ do
  my <- query myTable
  gFam <- groupBy $ my ! family'     -- Specify grouping key
  return $ gFam >< sum' (my ! age')  -- Aggregated results
~~~~~

Ordering
-----

Ordering key can be incrementally accumulated.

~~~~~ {#mycode .haskell}
personAndBirthdayO :: Relation () (Person, Birthday)
personAndBirthdayO =  relation $ do
  p <- query person
  b <- query birthday
  wheres $ p ! Person.name' .=. b ! Birthday.name'
  orderBy (b ! Birthday.day') Asc  -- Specify ordering key
  orderBy (p ! Person.name') Asc
  return $ p >< b
~~~~~

Ordering
-----

Ordering key context type (not aggregated or aggregated) is checked.

~~~~~ {#mycode .haskell}
agesOfFamiliesO :: Relation () (String, Maybe Int32)
agesOfFamiliesO =  aggregateRelation $ do
  my <- query myTable
  gFam <- groupBy $ my ! family'
  let s = sum' (my ! age')
  orderBy s Desc    -- Only aggregated value is allowed to pass
  orderBy gFam Asc
  return $ gFam >< s
~~~~~

Placeholders
-----

Can embed placeholder with typed.

~~~~~ {#mycode .haskell}
specifyPerson :: Relation String (Person, Birthday)
specifyPerson =  relation' $ do
  pb <- query personAndBirthday  -- Re-use pre-defined Relation
  (ph, ()) <- placeholder (\ph' -> wheres $ pb ! fst' ! Person.name' .=. ph')
  return (ph, pb)
~~~~~

Window Function
-----

Monadic style window building.

~~~~~ {#mycode .haskell}
ageRankOfFamilies :: Relation () ((Int64, String), Int32)
ageRankOfFamilies =  relation $ do
  my <- query myTable
  return $
    rank `over` do
      partitionBy $ my ! family'  -- Monad to build window
      orderBy (my ! age') Desc
    ><
    my ! family'
    ><
    my ! age'
~~~~~


Record Mapping
-----

Applicative style

~~~~~ {#mycode .haskell}
(|$|) :: (a -> b) -> p a -> p b
(|*|) :: p (a -> b) -> p a -> p b
~~~~~


Record Mapping
-----

Assign record type to SQL projection

~~~~~ {#mycode .haskell}
personAndBirthdayT :: Relation () PersonAndBirthday
personAndBirthdayT =  relation $ do
  p <- query person
  b <- query birthday
  wheres $ p ! Person.name' .=. b ! Birthday.name'
  return $ PersonAndBirthday |$| p |*| b  -- Build record phantom type

(|$|) :: (a -> b) -> Projection c a -> Projection c b
(|*|) :: Projection c (a -> b) -> Projection c a -> Projection c b
~~~~~

Record Mapping
-----

Projection path can be map to record.

~~~~~ {#mycode .haskell}
Birthday.day' :: Pi Birthday Day

uncurryPB :: Pi (Person, Birthday) PersonAndBirthday
uncurryPB =  PersonAndBirthday |$| fst' |*| snd'

(|$|) :: (a -> b) -> Pi r a -> Pi r b
(|*|) :: Pi r (a -> b) -> Pi r a -> Pi r b
~~~~~

Record Mapping
-----

Placeholder can be map to record.

~~~~~ {#mycode .haskell}
placeholder3 f =
  placeholder (\p0 -> placeholder (\p1 -> placeholder (\p2 -> f p0 p1 p2)))

personAndBirthdayP2 :: Relation ((String, Int32), String) PersonAndBirthday
personAndBirthdayP2 =  relation' $ do
  p <- query person
  b <- query birthday
  (ph0, (ph1, (ph2, ()))) <-
    placeholder3 (\ph0' ph1' ph2' ->
                   wheres $
                   (Person |$| p ! Person.name' |*| p ! Person.age' |*| p ! Person.address')
                   .=.
                   (Person |$| ph0' |*| ph1' |*| ph2') )
  return $ (ph0 >< ph1 >< ph2, PersonAndBirthday |$| p |*| b)
~~~~~

Record Mapping
-----

Record typed placeholder.

~~~~~ {#mycode .haskell}
personAndBirthdayP :: Relation Person PersonAndBirthday
personAndBirthdayP =  relation' $ do
  p <- query person
  b <- query birthday
  (ph, ()) <- placeholder (\ph' -> wheres $ p .=. ph')
  return $ (ph, PersonAndBirthday |$| p |*| b)
~~~~~

Relational Record and Opaleye
-----

* Notation differs
    * Opaleye -- Arrow notation
    * Relational Record -- Monad
* Not aggregated approach is same
* Differs on Aggregation, Ordering, Placeholder and Record mapping


Relational Record and Opaleye?
-----

Easy to wrap HRR combinators against arrow notations.

~~~~~ {#mycode .haskell}
personAndJoinA :: QuerySimple () (Projection Flat (Person, Birthday))
personAndJoinA =  proc () -> do
  p <- query -< person
  b <- query -< birthday
  wheres -< p ! Person.name' .=. b ! Birthday.name'
  returnA -< p >< b

personAndBirthdayOP :: Query (PersonColumn, BirthdayColumn)
personAndBirthdayOP = proc () -> do
  p   <- personQuery   -< ()
  b   <- birthdayQuery -< ()
  restrict -< Person.name p .== Birthday.name b
  returnA -< (p, b)
~~~~~

Discussion
-----

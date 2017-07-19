{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.Relational.QuickCheck.Arbitrary (
  Selector (..), D(..),
  VarExpr (..), varExprSQL, varExprHask,
  Expr (..),    exprSQL, exprHask,
  Cmp (..),     cmpSQL,  cmpHask,
  Term (..),    termSQL, termHask,
  Pred (..),    predSQL, predHask,
  Ranged (..),
  ) where

import Test.QuickCheck (Arbitrary (..), Gen, elements, listOf, frequency)
import Control.Applicative ((<$>), pure, (<*>))
import Data.Int (Int64)
import Database.Record
import Database.Relational.Query
import Database.Relational.Query.Pi.Unsafe (unsafeExpandIndexes)

import Test.Relational.QuickCheck.Model


-- | Integer record selector type which project
--   pure haskell integer value and SQL integer expression.
--   Type parameter 'r' is record type.
data Selector r =
  Selector
  { int  :: r -> Int64
  , sql  :: Pi r Int64
  }

instance PersistableWidth r => Show (Selector r) where
  show s = unwords ["Selector", show . unsafeExpandIndexes $ sql s]

genSelector :: [(r -> Int64, Pi r Int64)] -> Gen (Selector r)
genSelector = elements . map (uncurry Selector)

instance Arbitrary (Selector A) where
  arbitrary = genSelector [ (a0, a0'), (a1, a1'), (a2, a2') ]

instance Arbitrary (Selector B) where
  arbitrary = genSelector [ (b0, b0'), (b1, b1'), (b2, b2') ]


data Op
  = Plus
  | Minus
  deriving (Eq, Ord, Show)

instance Arbitrary Op where
  arbitrary = elements [Plus, Minus]

-- | Integer expression which has at least one record selector expression.
data VarExpr r
  = Column (Selector r)
  | VLeft  (VarExpr r) Op Int64
  | VRight Int64 Op (VarExpr r)
  deriving Show

data Expr r
  = Var (VarExpr r)
  | Expr r :+: Expr r
  | Expr r :-: Expr r
  deriving Show

data Cmp
  = Lt
  | Eq
  | Gt
  deriving (Eq, Ord, Show)

newtype Term a =
  Term (Expr a, Cmp, Expr a)
  deriving Show

data Pred a
 = PTerm (Term a)
 | Not (Pred a)
 | Pred a :&: Pred a
 | Pred a :|: Pred a
 deriving Show

-- | Wrapper to avoid undecidable instances
newtype D a =
  D { unD :: a }
  deriving Show

rangeN :: (Num a, Enum a) => a -> Gen a
rangeN n = elements [-n .. n]

genVarExpr :: Gen (Selector a) -> Gen (VarExpr a)
genVarExpr gs =
    frequency
    [ (3, Column <$> gs)
    , (1, VLeft  <$> rec' <*> arbitrary <*> rangeN 5)
    , (1, VRight <$> rangeN 5 <*> arbitrary <*> rec')
    ]
  where
    rec' = genVarExpr gs

genExpr :: Gen (Selector a) -> Gen (Expr a)
genExpr = gen . genVarExpr  where
  gen gv =
    frequency
    [ (3, Var <$> gv)
    , (1, (:+:) <$> rec' <*> rec')
    , (1, (:-:) <$> rec' <*> rec')
    ]   where  rec' = gen gv

instance Arbitrary Cmp where
  arbitrary =
    frequency
    [ (3, pure Lt)
    , (1, pure Eq)
    , (3, pure Gt)
    ]

genTerm :: Gen (Selector a) -> Gen (Term a)
genTerm = gen . genExpr  where
  gen ge = (Term <$>) $ (,,) <$> ge <*> arbitrary <*> ge

genPred :: Gen (Selector a) -> Gen (Pred a)
genPred = gen . genTerm  where
  gen gt =
    frequency
    [ (5, PTerm <$> gt)
    , (1, Not <$> rec')
    , (1, (:&:) <$> rec' <*> rec')
    , (1, (:|:) <$> rec' <*> rec')
    ]  where  rec' = gen gt

instance Arbitrary (Selector a) => Arbitrary (D (VarExpr a)) where
  arbitrary = D <$> genVarExpr arbitrary

instance Arbitrary (Selector a) => Arbitrary (D (Expr a)) where
  arbitrary = D <$> genExpr arbitrary

instance Arbitrary (Selector a) => Arbitrary (D (Term a)) where
  arbitrary = D <$> genTerm arbitrary

instance Arbitrary (Selector a) => Arbitrary (D (Pred a)) where
  arbitrary = D <$> genPred arbitrary


opSQL :: Op
      -> Projection Flat Int64
      -> Projection Flat Int64
      -> Projection Flat Int64
opSQL = d  where
  d Plus   = (.+.)
  d Minus  = (.-.)

varExprSQL :: PersistableWidth a
           => Projection Flat a
           -> VarExpr a
           -> Projection Flat Int64
varExprSQL r  =  d  where
  d (Column s)       =  r ! sql s
  d (VLeft  ve op i)  =  opSQL op (d ve) (value i)
  d (VRight i op ve)  =  opSQL op (value i) (d ve)

exprSQL :: PersistableWidth a
        => Projection Flat a
        -> Expr a
        -> Projection Flat Int64
exprSQL r  =  d  where
  d (Var ve)     =  varExprSQL r ve
  d (e0 :+: e1)  =  d e0 .+. d e1
  d (e0 :-: e1)  =  d e0 .-. d e1

cmpSQL :: Cmp
       -> Projection Flat a
       -> Projection Flat a
       -> Projection Flat (Maybe Bool)
cmpSQL = d  where
  d Lt = (.<.)
  d Eq = (.=.)
  d Gt = (.>.)

termSQL :: PersistableWidth a
        => Projection Flat a
        -> Term a
        -> Projection Flat (Maybe Bool)
termSQL r (Term (e0, op, e1))  =  cmpSQL op (exprSQL r e0) (exprSQL r e1)

predSQL :: PersistableWidth a
        => Projection Flat a
        -> Pred a
        -> Projection Flat (Maybe Bool)
predSQL r = d  where
  d (PTerm t)    =  termSQL r t
  d (Not p)      =  not' $ d p
  d (p0 :&: p1)  =  d p0 `and'` d p1
  d (p0 :|: p1)  =  d p0 `or'` d p1

opHask :: Num a
       => Op -> a -> a -> a
opHask  =  d where
  d Plus  =  (+)
  d Minus =  (-)

varExprHask :: r
            -> VarExpr r
            -> Int64
varExprHask r = d  where
  d (Column s)        =  int s r
  d (VLeft ve op i)   =  opHask op (d ve) i
  d (VRight i op ve)  =  opHask op i (d ve)

exprHask :: r
         -> Expr r
         -> Int64
exprHask r = d  where
  d (Var ve)     =  varExprHask r ve
  d (e0 :+: e1)  =  d e0 + d e1
  d (e0 :-: e1)  =  d e0 - d e1

cmpHask :: Ord a
        => Cmp
        -> a -> a -> Bool
cmpHask = d  where
  d Lt  =  (<)
  d Eq  =  (==)
  d Gt  =  (>)

termHask :: Ord a
         => a
         -> Term a
         -> Bool
termHask r (Term (e0, op, e1))  =  cmpHask op (exprHask r e0) (exprHask r e1)

predHask :: Ord a
         => a
         -> Pred a
         -> Bool
predHask r = d  where
  d (PTerm t)    =  termHask r t
  d (Not p)      =  not $ d p
  d (p0 :&: p1)  =  d p0 && d p1
  d (p0 :|: p1)  =  d p0 || d p1


newtype Ranged a = Ranged { runRanged :: [a] }
  deriving (Eq, Show)

instance Arbitrary (Ranged A) where
  arbitrary =
    (Ranged <$>) . listOf $
    A
    <$> rangeN 5
    <*> rangeN 5
    <*> rangeN 5

instance Arbitrary (Ranged B) where
  arbitrary =
    (Ranged <$>) . listOf $
    B
    <$> rangeN 5
    <*> rangeN 5
    <*> rangeN 5

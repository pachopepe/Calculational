{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Calculational.MonoidExt
Description : Expressions parser
Copyright   : (c) Francisco J Cháves, 2012
License     : MIT
Maintainer  : pachopepe@gmail.com
Stability   : experimental

A expression parser for Dijkstra-Sholten style like expressions. 
-}
module Calculational.MonoidExt
( bind
, average
, Equiv(..)
, NEquiv(..)
, Infty(..)
, Average(..)
, Union (..)
, Universe (..)
, Intersection (..)
, IntersectionClass (..)
, UnionClass(..)
)
where

import GHC.Exts (Constraint(..))
import Data.Semigroup
import Data.Monoid
import Data.Ratio
import Control.Applicative as A
import Control.Monad
import Data.Foldable as F
import Data.List as L
import Data.Set as Set
import Data.MultiSet as MultiSet hiding (bind)

-- | Boolean monoid under equivalence
newtype Equiv  = Equiv { getEquiv :: Bool }
               deriving (Eq, Ord, Read, Show, Bounded)

-- | Boolean monoid under inequivalence
newtype NEquiv  = NEquiv { getNEquiv :: Bool }
                deriving (Eq, Ord, Read, Show, Bounded)

instance Semigroup Equiv where
     (Equiv x) <> (Equiv y) = Equiv (x == y)
     

instance Monoid Equiv where
     mempty = Equiv True

instance Semigroup NEquiv where
     (NEquiv x) <> (NEquiv y) = NEquiv (x /= y)

instance Monoid NEquiv where
     mempty = NEquiv False

-- | The 'bind' operator is the 'foldMap' function swapping
-- his arguments
bind :: (Monoid m, Foldable t) => t a -> (a -> m) -> m
bind = flip foldMap

-- | Monoid wrapper
newtype Join m = Join { getJoin :: m }
                deriving (Eq, Ord, Read, Show, Bounded)

-- | Monoid under union
newtype Union m = Union { getUnion :: m }
                deriving (Eq, Ord, Read, Show, Bounded)

-- | Monoid under union
class UnionClass m a where
    type UC m a :: Constraint
    munion :: UC m a => m a -> m a -> m a    -- ^ Union overloading function

instance (UC m a,UnionClass m a) => UnionClass (Universe m) a where
      type UC (Universe m) a = UC m a
      Container xs `munion` Container ys = Container (xs `munion` ys)
      _ `munion` _ = Universe

instance (UC m a,UnionClass m a,Semigroup (m a)) => Semigroup (Union (m a)) where
      s1 <> s2 = Union (getUnion s1 `munion` getUnion s2) 

instance (UC m a,UnionClass m a,Monoid (m a)) => Monoid (Union (m a)) where
      mempty = Union mempty

instance UnionClass [] a where
      type UC [] a = Eq a
      munion = L.union

instance UnionClass Set.Set a where
      type UC Set.Set a = Ord a
      munion = Set.union

instance UnionClass MultiSet.MultiSet a where
      type UC MultiSet.MultiSet a = Ord a
      munion = MultiSet.union

-- | Monoid under intersection
-- The unit of intersection must be given
newtype Intersection m = Intersection { getIntersection :: m }
                       deriving (Eq, Ord, Read, Show, Bounded)

-- | Monoid under intersection
class IntersectionClass m a where
    type IC m a :: Constraint
    mintersection :: (IC m a) => m a -> m a -> m a -- ^ Intersection overloading function

instance (IC m a,IntersectionClass m a,Bounded (m a),Eq a,Ord a) => Semigroup (Intersection (m a)) where
      s1 <> s2 = Intersection (getIntersection s1 `mintersection` getIntersection s2) 

instance (IC m a,IntersectionClass m a,Bounded (m a),Eq a,Ord a) => Monoid (Intersection (m a)) where
      mempty = Intersection maxBound

instance IntersectionClass [] a where
      type IC [] a = Eq a
      mintersection = L.intersect

instance IntersectionClass Set.Set a where
      type IC Set.Set a = Ord a
      mintersection = Set.intersection

instance IntersectionClass MultiSet.MultiSet a where
      type IC MultiSet.MultiSet a = Ord a
      mintersection = MultiSet.intersection

instance (IC m a,IntersectionClass m a) => IntersectionClass (Universe m) a where
      type IC (Universe m) a = IC m a
      Container xs `mintersection` Container ys = Container (xs `mintersection` ys)
      Universe `mintersection` ys = ys
      xs  `mintersection` Universe = xs

instance Semigroup m => Semigroup (Join m) where
        Join x <> Join y = Join (x <> y)

instance (Monoid m) => Monoid (Join m) where
        mempty = Join mempty

-- | Data type wrapper used to bound infinity unbounded data types, 
-- like integers.
data Infty a = NegInfty
             | Value { getValue :: a }
             | PosInfty
             deriving (Eq, Ord, Read, Show)

instance Bounded (Infty a) where 
    minBound = NegInfty
    maxBound = PosInfty

-- | Data type used to add an upper bound to containers
data Universe m a = Container { getContainer :: m a }
                  | Universe
                  deriving (Eq, Ord, Read, Show)

instance Monoid (m a) => Bounded (Universe m a) where 
    minBound = Container mempty
    maxBound = Universe

instance Functor m => Functor (Universe m) where 
    fmap f (Container xs) = Container (fmap f xs) 

instance Applicative m => Applicative (Universe m) where
    pure = Container . pure
    Container fs <*> Container xs = Container (fs <*> xs)
    _ <*> _ = Universe 

instance (Alternative m) => Alternative (Universe m) where
    empty = Container (A.empty)
    Container xs <|> Container ys = Container (xs <|> ys)
    Universe <|> _ = Universe
    _ <|> Universe = Universe

instance (Semigroup (m a)) => Semigroup (Universe m a) where
    (Container xs) <> (Container ys) = Container (xs <> ys)
    _ <> _ = Universe

instance (Monoid (m a)) => Monoid (Universe m a) where
    mempty = Container mempty

instance (MonadPlus m, Foldable m) => Monad (Universe m) where
    return = Container . return
    c@(Container xs) >>= f = F.foldr (\x ys -> f x `mplus` ys) (Container mzero) xs                                            
    Universe >>= _ = Universe

instance (Foldable m, MonadPlus m) => MonadPlus (Universe m) where
    mzero = Container mzero
    (Container xs) `mplus` (Container ys) = Container (xs `mplus` ys)
    _ `mplus` _ = Universe

instance Foldable m => Foldable (Universe m) where
    foldr f z (Container xs) = F.foldr f z xs
    foldr f z Universe = z

-- | The average is not a monoid, but can be modeled as a
-- monoid maded by pairs of elements of the Sum monoid.
-- The first element of the pair have the sum of the elements
-- and the second the number of elements
newtype Average a = Average { getAverage :: (Sum a,Sum a) } 
                  deriving (Read, Show)

instance Num a => Semigroup (Average a) where
  Average x <> Average y = Average (x <> y)
  
instance Num a => Monoid (Average a) where
  mempty = Average (mempty,mempty)

-- | @average@ takes the result of a Sum monoid and a Count monoid
-- and obtains the average dividing both results 
average :: (Real a,Fractional b) => Average a -> b
average (Average (Sum sum,Sum cnt)) = fromRational (toRational sum / toRational cnt)


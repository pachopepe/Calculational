{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- | Módulo Quantifier.MonoidExt
-- Monoid extensions to be used with quantifiers
--
-- Autor: Francsco J. Cháves
--
-- 
-----------------------------------------------------------------------------

module Calculational.MonoidExt
( bind
, average
, Product(..)
, Sum(..)
, All(..)
, Any(..)
, Equiv(..)
, NEquiv(..)
, Infty(..)
, Maximum(..)
, Minimum(..)
, Average(..)
, Union (..)
, Universe (..)
, Intersection (..)
, IntersectionClass (..)
, UnionClass(..)
)
where

import GHC.Prim (Constraint(..))
import Data.Monoid
import Data.Ratio
import Control.Applicative
import Control.Monad
import Data.Foldable as F
import Data.List as L
import qualified Data.Set as Set
import qualified Data.MultiSet as MultiSet

-- | Boolean monoid under equivalence
newtype Equiv  = Equiv { getEquiv :: Bool }
               deriving (Eq, Ord, Read, Show, Bounded)

-- | Bolean monoid under inequivalence
newtype NEquiv  = NEquiv { getNEquiv :: Bool }
                deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid Equiv where
     mempty = Equiv True
     (Equiv x) `mappend` (Equiv y) = Equiv (x == y)

instance Monoid NEquiv where
     mempty = NEquiv False
     (NEquiv x) `mappend` (NEquiv y) = NEquiv (x /= y)

-- | the 'bind' operator is the 'foldMap' function swapping
-- his arguments
bind :: (Monoid m, Foldable t) => t a -> (a -> m) -> m
bind = flip foldMap

-- | Monoid wrapper
newtype Join m = Join { getJoin :: m }
                deriving (Eq, Ord, Read, Show, Bounded)

-- | Monoid under union
newtype Union m = Union { getUnion :: m }
                deriving (Eq, Ord, Read, Show, Bounded)

class UnionClass m a where
    type UC m a :: Constraint
    munion :: UC m a => m a -> m a -> m a

instance (UC m a,UnionClass m a) => UnionClass (Universe m) a where
      type UC (Universe m) a = UC m a
      Container xs `munion` Container ys = Container (xs `munion` ys)
      _ `munion` _ = Universe

instance (UC m a,UnionClass m a,Monoid (m a)) => Monoid (Union (m a)) where
      mempty = Union mempty
      s1 `mappend` s2 = Union (getUnion s1 `munion` getUnion s2) 

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

class IntersectionClass m a where
    type IC m a :: Constraint
    mintersection :: (IC m a) => m a -> m a -> m a

instance (IC m a,IntersectionClass m a,Bounded (m a),Eq a,Ord a) => Monoid (Intersection (m a)) where
      mempty = Intersection maxBound
      s1 `mappend` s2 = Intersection (getIntersection s1 `mintersection` getIntersection s2) 

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

instance (Monoid m) => Monoid (Join m) where
        mempty = Join mempty
        Join x `mappend` Join y = Join (x `mappend` y)

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
    empty = Container empty
    Container xs <|> Container ys = Container (xs <|> ys)
    Universe <|> _ = Universe
    _ <|> Universe = Universe

instance (Monoid (m a)) => Monoid (Universe m a) where
    mempty = Container mempty
    (Container xs) `mappend` (Container ys) = Container (xs `mappend` ys)
    _ `mappend` _ = Universe

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

-- Monoid under maximum
-- The type 'a' must be bounded
newtype Maximum a = Maximum { getMaximum :: a }
                  deriving (Eq, Ord, Read, Show, Bounded)

-- | Monoid under minimum
-- The type 'a' must be bounded
newtype Minimum a = Minimum { getMinimum :: a }
                  deriving (Eq, Ord, Read, Show, Bounded)

instance (Bounded a,Ord a) => Monoid (Maximum a) where
        mempty = Maximum minBound
        Maximum x `mappend` Maximum y = Maximum (x `max` y)

instance (Bounded a,Ord a) => Monoid (Minimum a) where
        mempty = Minimum maxBound
        Minimum x `mappend` Minimum y = Minimum (x `min` y)

-- | The average is not a monoid, but can be modeled as a
-- monoid maded by pairs of elements of the Sum monoid.
-- The first element of the pair have the sum of the elements
-- and the second the number of elements
newtype Average a = Average { getAverage :: (Sum a,Sum a) } 
                  deriving (Read, Show)

instance Num a => Monoid (Average a) where
  mempty = Average (mempty,mempty)
  Average x `mappend` Average y = Average (x `mappend` y)

-- | @average@ takes the result of a Sum monoid and a Count monoid
-- and obtains the average dividing both results 
average :: (Real a,Fractional b) => Average a -> b
average (Average (Sum sum,Sum cnt)) = fromRational (toRational sum / toRational cnt)


{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Calculational.Definitions(
  implies
  , Iver(..)
  , CollectionClass(..)
  )
where 

import GHC.Prim (Constraint(..))
import Data.List as L
import qualified Data.Set as Set
import qualified Data.MultiSet as MultiSet

implies :: Bool -> Bool -> Bool
implies p q = (not p) || q

class Iver a where
  iver :: Num b => a -> b

instance Iver Bool where
  iver True  = 1
  iver False = 0

instance Iver ([] a) where
  iver = L.genericLength

instance Iver (Set.Set a) where
  iver = fromIntegral . Set.size

instance Iver (MultiSet.MultiSet a) where
  iver = fromIntegral . MultiSet.size

class CollectionClass m a where
    type DC m a :: Constraint
    size :: m a -> Int
    member :: DC m a => a -> m a -> Bool
    union :: DC m a => m a -> m a -> m a
    intersection :: DC m a => m a -> m a -> m a
    difference :: DC m a => m a -> m a -> m a
    subset :: DC m a => m a -> m a -> Bool
    subsetEq :: DC m a => m a -> m a -> Bool
    superSet :: DC m a => m a -> m a -> Bool
    superSet = flip subset
    superSetEq :: DC m a => m a -> m a -> Bool
    superSetEq = flip subsetEq

instance CollectionClass [] a where
      type DC [] a = Eq a
      size = L.length
      member = L.elem
      union = L.union
      intersection = L.intersect
      difference = (L.\\)
      subset xs ys = (xs /= ys) && xs `subsetEq` ys
      subsetEq [] _ = True
      subsetEq _ [] = False
      subsetEq (x:xs) (y:ys) = if x == y
                               then xs `subsetEq` ys
                               else (x:xs) `subsetEq` ys

instance CollectionClass Set.Set a where
      type DC Set.Set a = Ord a
      size = Set.size
      member = Set.member
      union = Set.union
      intersection = Set.intersection
      difference = (Set.\\)
      subsetEq = Set.isSubsetOf
      subset = Set.isProperSubsetOf


instance CollectionClass MultiSet.MultiSet a where
      type DC MultiSet.MultiSet a = Ord a
      size = MultiSet.size
      member = MultiSet.member
      union = MultiSet.union
      intersection = MultiSet.intersection
      difference = (MultiSet.\\)
      subset = MultiSet.isProperSubsetOf
      subsetEq = MultiSet.isSubsetOf


{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Calculational.Definitions(
  implies
  , Sharp(..)
  , CollectionClass(..)
  )
where 

import GHC.Prim (Constraint(..))
import Data.List as L
import qualified Data.Set as Set
import qualified Data.MultiSet as MultiSet

-- | The implication operator @p => q@ 
implies :: Bool -> Bool -> Bool
implies p q = (not p) || q

-- | The collections operations
class CollectionClass m a where
    type DC m a :: Constraint
    size :: m a -> Int
    member :: DC m a => a -> m a -> Bool
    notMember  :: DC m a => a -> m a -> Bool
    notMember x s = not (x `member` s)
    union :: DC m a => m a -> m a -> m a
    intersection :: DC m a => m a -> m a -> m a
    difference :: DC m a => m a -> m a -> m a
    subset :: DC m a => m a -> m a -> Bool
    notSubset :: DC m a => m a -> m a -> Bool
    notSubset u s = not (u `subset` s)
    subsetEq :: DC m a => m a -> m a -> Bool
    notSubsetEq :: DC m a => m a -> m a -> Bool
    notSubsetEq u s = not (u `subsetEq` s)
    superSet :: DC m a => m a -> m a -> Bool
    superSet = flip subset
    notSuperSet :: DC m a => m a -> m a -> Bool
    notSuperSet u s = not (u `superSet` s)
    superSetEq :: DC m a => m a -> m a -> Bool
    superSetEq = flip subsetEq
    notSuperSetEq :: DC m a => m a -> m a -> Bool
    notSuperSetEq u s = not (u `superSetEq` s)
    emptyCollection :: DC m a => m a 

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
      emptyCollection = []

instance CollectionClass Set.Set a where
      type DC Set.Set a = Ord a
      size = Set.size
      member = Set.member
      union = Set.union
      intersection = Set.intersection
      difference = (Set.\\)
      subsetEq = Set.isSubsetOf
      subset = Set.isProperSubsetOf
      emptyCollection = Set.empty


instance CollectionClass MultiSet.MultiSet a where
      type DC MultiSet.MultiSet a = Ord a
      size = MultiSet.size
      member = MultiSet.member
      union = MultiSet.union
      intersection = MultiSet.intersection
      difference = (MultiSet.\\)
      subset = MultiSet.isProperSubsetOf
      subsetEq = MultiSet.isSubsetOf
      emptyCollection = MultiSet.empty

-- | Used for overload the sharp @#@ operator
class Sharp a where
  sharp :: Num b => a -> b

-- | Sharpson applied to booleans is the iverson operatot: 
-- @
--     sharp True = 1
--     sharp False = 0
-- @ 
instance Sharp Bool where
  sharp True  = 1
  sharp False = 0

-- | Sharp applied to a finite container gets his length 
instance CollectionClass c a => Sharp (c a) where
  sharp = fromIntegral . size


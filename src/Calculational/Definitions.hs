{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}

{-|
Module      : Calculational.Definitions
Description : Collection and Sharp class definitions.
Copyright   : (c) Francisco J Cháves, 2012
License     : MIT
Maintainer  : pachopepe@gmail.com
Stability   : experimental

Definitons to support Dijkstra style like expressions. 
-}

module Calculational.Definitions(
  implies
  , Sharp(..)
  , CollectionClass(..)
  )
where 

import GHC.Exts (Constraint(..))

import Data.List as L
import Data.Set as Set
import Data.MultiSet as MultiSet

-- | The implication operator @p => q@ 
implies :: Bool -> Bool -> Bool
implies p q = (not p) || q

-- | The collections operations
class CollectionClass m a where
    type DC m a :: Constraint  
    size :: m a -> Int         -- ^ Collection size
    member :: DC m a => a -> m a -> Bool -- ^ Collection membership function
    notMember  :: DC m a => a -> m a -> Bool -- ^ Collection not membership function
    notMember x s = not (x `Calculational.Definitions.member` s) 
    union :: DC m a => m a -> m a -> m a  -- ^ Collection union
    intersection :: DC m a => m a -> m a -> m a -- ^ Collection intersection
    difference :: DC m a => m a -> m a -> m a   -- ^ Collection difference
    subset :: DC m a => m a -> m a -> Bool   -- ^ Collection subset relation
    notSubset :: DC m a => m a -> m a -> Bool   -- ^ Collection not subset relation
    notSubset u s = not (u `subset` s)
    subsetEq :: DC m a => m a -> m a -> Bool   -- ^ Collection subset or equal relation
    notSubsetEq :: DC m a => m a -> m a -> Bool   -- ^ Collection not subset or equal relation
    notSubsetEq u s = not (u `subsetEq` s)
    superSet :: DC m a => m a -> m a -> Bool   -- ^ Collection superset relation
    superSet = flip subset
    notSuperSet :: DC m a => m a -> m a -> Bool   -- ^ Collection not superset relation
    notSuperSet u s = not (u `superSet` s)
    superSetEq :: DC m a => m a -> m a -> Bool   -- ^ Collection superset or equal relation
    superSetEq = flip subsetEq
    notSuperSetEq :: DC m a => m a -> m a -> Bool   -- ^ Collection not superset or equal relation
    notSuperSetEq u s = not (u `superSetEq` s)
    emptyCollection :: DC m a => m a    -- ^ empty Collection

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
  sharp :: Num b => a -> b -- ^ the sharp @#@ function

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
  sharp = fromIntegral . Calculational.Definitions.size


{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-|
Module      : Calculational
Description : Dijkstra-Sholten style like expressions.
Copyright   : (c) Francisco J Cháves, 2012
License     : MIT
Maintainer  : pachopepe@gmail.com
Stability   : experimental

A Dijkstra-Sholten style like expressions that execute. 
-}
module Calculational( 
  calc
, average
, bind
, Equiv(..)
, NEquiv(..)
, Infty(..)
, Maximum(..)
, Minimum(..)
, Average(..)
, Universe(..)
, module Calculational.Definitions 
, module Data.Monoid
-- , module Data.Set -- Set.Set(..)
-- , module Data.MultiSet -- MultiSet.MultiSet(..)
)
where
  
import Data.Monoid
import Calculational.QuasiQuoter
import Calculational.MonoidExt
import Calculational.Definitions

-- import Data.Set -- as Set
-- import Data.MultiSet -- as MultiSet

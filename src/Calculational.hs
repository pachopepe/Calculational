{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Calculational( 
  calc
, average
, bind
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
, Universe(..)
, module Calculational.Definitions 
, Set.Set(..)
, MultiSet.MultiSet(..)
)
where
  
import Data.Monoid
import Calculational.QuasiQuoter
import Calculational.MonoidExt
import Calculational.Definitions

import qualified Data.Set as Set
import qualified Data.MultiSet as MultiSet

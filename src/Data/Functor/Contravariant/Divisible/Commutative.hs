{-# language MonoLocalBinds #-}
module Data.Functor.Contravariant.Divisible.Commutative where

import Control.Commutative
import Data.Semigroup.Commutative
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Compose
import Data.Functor.Contravariant.Divisible

class Divisible f => CommutativeDivisible f

instance CommutativeDivisible Predicate

instance CommutativeDivisible Equivalence

instance CommutativeMonoid a => CommutativeDivisible (Op a)

instance 
  ( CommutativeDivisible f
  , Commutative g
  ) => CommutativeDivisible (ComposeCF f g)

instance 
  ( Commutative f
  , CommutativeDivisible g
  ) => CommutativeDivisible (ComposeFC f g) 

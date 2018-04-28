{-# language GADTs #-}
{-# language CPP #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
module Data.Semigroup.Commutative 
  ( CommutativeSemigroup
  , CommutativeMonoid
  ) where

import Data.Functor.Identity
import Data.Monoid as Monoid hiding ((<>))
import Data.Proxy
import Data.Semigroup as Semigroup
import Data.Set
import Data.Tagged
import Data.Void
import GHC.Event

-- | 
-- @
-- ('<>') = 'flip' ('<>')
-- @
class Semigroup m => CommutativeSemigroup m

instance CommutativeSemigroup Void
instance CommutativeSemigroup Any
instance CommutativeSemigroup All
instance Ord a => CommutativeSemigroup (Min a)
instance Ord a => CommutativeSemigroup (Max a)
instance Ord a => CommutativeSemigroup (Set a)
instance CommutativeSemigroup a => CommutativeSemigroup (Option a)
instance CommutativeSemigroup a => CommutativeSemigroup (Dual a)
instance CommutativeSemigroup a => CommutativeSemigroup (Maybe a)
instance Num a => CommutativeSemigroup (Sum a) -- a bit hinky
instance CommutativeMonoid m => CommutativeSemigroup (WrappedMonoid m)
instance CommutativeSemigroup ()
-- events
instance CommutativeSemigroup Event
instance CommutativeSemigroup Lifetime
instance CommutativeSemigroup a => CommutativeSemigroup (Identity a)
instance CommutativeSemigroup a => CommutativeSemigroup (x -> a)
instance (CommutativeSemigroup a, CommutativeSemigroup b) => CommutativeSemigroup (a,b)
instance (CommutativeSemigroup a, CommutativeSemigroup b, CommutativeSemigroup c) => CommutativeSemigroup (a,b,c)
instance (CommutativeSemigroup a, CommutativeSemigroup b, CommutativeSemigroup c, CommutativeSemigroup d) => CommutativeSemigroup (a,b,c,d)
instance (CommutativeSemigroup a, CommutativeSemigroup b, CommutativeSemigroup c, CommutativeSemigroup d, CommutativeSemigroup e) => CommutativeSemigroup (a,b,c,d,e)
instance CommutativeSemigroup (Proxy a)
instance CommutativeSemigroup a => CommutativeSemigroup (Tagged s a)

-- 8.6
-- instance (Commutative f, CommutativeSemigroup m) => CommutativeSemigroup (Ap f m) -- this will force a cycle

class (CommutativeSemigroup m, Monoid m) => CommutativeMonoid m
instance (CommutativeSemigroup m, Monoid m) => CommutativeMonoid m

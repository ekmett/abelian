{-# language GADTs #-}
{-# language TypeOperators #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}

module Control.Commutative 
  ( Commutative
  , CommutativeMonad
  ) where

import Control.Applicative (ZipList, Const)
import Control.Applicative.Backwards
-- import Control.Arrow (Kleisli)
import Control.Comonad
import Control.Comonad.Env
import Control.Comonad.Traced
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Data.Complex
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Product as Functor
import Data.Functor.Reverse
import Data.Monoid as Monoid
import Data.Proxy
import Data.Semigroup as Semigroup
import Data.Semigroup.Commutative
import Data.Tagged
import GHC.Generics
import Linear
import Linear.Plucker
import Linear.V


-- |
-- @
-- ('<*>') = 'flip' ('<**>')
-- @

class Applicative m => Commutative m

instance (Commutative f, Commutative g) => Commutative (Compose f g)
instance (Commutative m, CommutativeMonoid w) => Commutative (Strict.WriterT w m)
instance (Commutative m, CommutativeMonoid w) => Commutative (Lazy.WriterT w m)
instance CommutativeMonoid a => Commutative ((,) a)

-- instance (CommutativeMonoid a, CommutativeMonoid b) => Commutative ((,,) a b)
-- instance (CommutativeMonoid a, CommutativeMonoid b, CommutativeMonoid c) => Commutative ((,,,) a b c)
-- instance (CommutativeMonoid a, CommutativeMonoid b, CommutativeMonoid c, CommutativeMonoid d) => Commutative ((,,,,) a b c d)
instance Commutative ((->) e)
instance Commutative m => Commutative (ReaderT e m)
instance Commutative V4
instance Commutative V3
instance Commutative V2
instance Commutative Linear.V1
instance Commutative V0
instance Commutative Plucker
instance Dim n => Commutative (V n)
instance Commutative Complex
instance Commutative Quaternion
instance Commutative Identity
instance Commutative m => Commutative (IdentityT m)
instance (Commutative m, Commutative n) => Commutative (Functor.Product m n)
instance CommutativeMonad m => Commutative (MaybeT m)
instance Commutative m => Commutative (Reverse m)
instance Commutative m => Commutative (Backwards m)
instance CommutativeMonoid a => Commutative (Const a)
instance Commutative (Tagged a)
instance Commutative Proxy
-- instance Commutative m => Commutative (Kleisli m a)
instance Commutative (Cokleisli w a)
instance Commutative ZipList
instance Commutative f => Commutative (Alt f)
instance Commutative Semigroup.Last -- NB: Not CommutativeMonoid!
instance Commutative Semigroup.First -- NB: Not CommutativeMonoid!
instance Commutative Monoid.Last -- NB: Not CommutativeMonoid!
instance Commutative Monoid.First -- NB: Not CommutativeMonoid!
instance Commutative Monoid.Product -- NB: Not CommutativeMonoid!
instance Commutative Monoid.Sum -- NB: Not CommutativeMonoid!
instance Commutative Option -- NB: Not CommutativeMonoid!
instance Commutative m => Commutative (M1 i c m)
instance (Commutative f, Commutative g) => Commutative (f :.: g)
instance (Commutative f, Commutative g) => Commutative (f :*: g)
-- instance CommutativeSemigroup c => Commutative (K1 i c) -- missing in base 
instance Commutative f => Commutative (Rec1 f)
instance Commutative Par1
instance Commutative U1
instance (Commutative w, CommutativeMonoid e) => Commutative (EnvT e w)
instance Commutative w => Commutative (TracedT e w)
-- instance Commutative f => Commutative (Ap f) -- ghc 8.6

class    (Commutative m, Monad m) => CommutativeMonad m
instance (Commutative m, Monad m) => CommutativeMonad m

{-# language TypeOperators #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language MonoLocalBinds #-}
module Data.Functor.Comm 
  ( Comm
  , CommBind
  ) where

import Control.Applicative (ZipList, Const)
import Control.Applicative.Backwards
import Control.Commutative
import Control.Comonad
import Control.Comonad.Env
import Control.Comonad.Traced
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Data.Complex
import Data.Functor.Bind
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Product as Functor
import Data.Functor.Reverse
import Data.Map.Lazy
import Data.Monoid as Monoid
import Data.Proxy
import Data.Semigroup as Semigroup
import Data.Semigroup.Commutative
import Data.Tagged
import GHC.Generics as Generics
import Linear
import Linear.Plucker
import Linear.V

-- |
-- @
-- ('<.>') = 'flip' ('<..>')
-- @
class Apply f => Comm f

instance (Comm f, Comm g) => Comm (Compose f g)
instance (Comm m, CommutativeSemigroup w) => Comm (Strict.WriterT w m)
instance (Comm m, CommutativeSemigroup w) => Comm (Lazy.WriterT w m)
instance CommutativeSemigroup a => Comm ((,) a)

-- instance (CommutativeSemigroup a, CommutativeSemigroup b) => Comm ((,,) a b)
-- instance (CommutativeSemigroup a, CommutativeSemigroup b, CommutativeSemigroup c) => Comm ((,,,) a b c)
-- instance (CommutativeSemigroup a, CommutativeSemigroup b, CommutativeSemigroup c, CommutativeSemigroup d) => Comm ((,,,,) a b c d)

instance Comm ((->) e)
instance Comm m => Comm (ReaderT e m)
instance Comm V4
instance Comm V3
instance Comm V2
instance Comm Linear.V1
instance Comm V0
instance Comm Plucker
instance Dim n => Comm (V n)
instance Comm Complex
instance Comm Quaternion
instance Comm Identity
instance Comm m => Comm (IdentityT m)
instance (Comm m, Comm n) => Comm (Functor.Product m n)
instance CommutativeMonad m => Comm (MaybeT m)
instance Comm m => Comm (Reverse m)
instance Comm m => Comm (Backwards m)
instance CommutativeSemigroup a => Comm (Const a)
instance Comm (Tagged a)
instance Comm Proxy
-- instance Comm m => Comm (Kleisli m a)
instance Comm (Cokleisli w a)
instance Comm ZipList
instance Comm Option -- NB: Not CommutativeSemigroup!
-- semigroupoids missing instances:
instance Comm f => Comm (Alt f)
instance Comm Semigroup.Last -- NB: Not CommutativeSemigroup!
instance Comm Semigroup.First -- NB: Not CommutativeSemigroup!
instance Comm Monoid.Last -- NB: Not CommutativeSemigroup!
instance Comm Monoid.First -- NB: Not CommutativeSemigroup!
instance Comm Monoid.Product -- NB: Not CommutativeSemigroup!
instance Comm Monoid.Sum -- NB: Not CommutativeSemigroup!

instance Comm m => Comm (M1 i c m)
instance (Comm f, Comm g) => Comm (f :.: g)
instance (Comm f, Comm g) => Comm (f :*: g)
instance CommutativeSemigroup c => Comm (K1 i c) -- missing in base 
instance Comm f => Comm (Rec1 f)
instance Comm Par1
instance Comm U1
instance (Comm w, CommutativeSemigroup e) => Comm (EnvT e w)
instance Comm w => Comm (TracedT e w)
-- instance Comm f => Comm (Ap f) -- ghc 8.6

class    (Comm m, Bind m) => CommBind m
instance (Comm m, Bind m) => CommBind m

-- instances that don't extend to Commutative
instance Ord k => Comm (Map k)
instance Comm Generics.V1

{-# language GADTs #-}

module Control.Commutative 
  ( Commutative
  , CommutativeSemigroup
  , CommutativeMonoid
  ) where

import Control.Applicative (liftA2)
import Control.Monad ((>=>), ap)
import Data.Map.Lazy as Map

{-
-- crippled Data.Functor.Apply style
class Apply m => Comm m
instance Ord k => Comm (Map k)
instance Comm IntMap
instance Comm V1

-- crippled Data.Functor.Bind style
class    (Comm m, Bind m) => CommBind m
instance (Comm m, Bind m) => CommBind m
-}

{-
-- we can finally fix naïve ListT
newtype ListT m a = ListT { runListT :: m [a] }
  deriving Functor

instance Commutative m => Applicative (ListT m)
instance Commutative m => Monad (ListT m)
-}

-- instance Commutative Par -- big dep

-- comonads
instance (Commutative w, CommutativeMonoid e) => Commutative (EnvT e w)
instance Commutative w => Commutative (TracedT e m)


{-
newtype Mealy a b = Mealy { runMealy :: a -> (b, Mealy a b) }
  deriving Functor

instance Applicative (Mealy a) where
  pure a = x where x = Mealy $ \_ -> (a, x)
  Mealy mf <*> Mealy ma = Mealy $ \x -> 
    let (f, mf') = mf x
        (a, ma') = ma x
    in (f a, mf' <*> ma')

instance Commutative (Mealy a)

data Moore a b where
   Moore :: (r -> b) -> (r -> a -> r) -> r -> Moore a b

data Complex a = a :+ a

data StrictPair a b = !a :* !b

instance Applicative (Moore a) where
  pure a = Moore (const a) const ()
  Moore xf bxx xz <*> Moore ya byy yz = Moore
    (\(x :* y) -> xf x $ ya y)
    (\(x :* y) b -> bxx x b :* byy y b))
    (xz :* yz)

instance Commutative (Moore a)

-- free monad given a commutative applicative
data Free m a where
  Pure :: a -> Free m a
  Free :: m x -> (x -> Free m a) -> Free m a

instance Functor (Free m) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Free mf k) = Free mf (fmap f . k)
  
instance Commutative m => Applicative (Free m) where
  pure = Pure
  Pure f <*> ma = fmap f ma
  Free mf kf <*> Pure a = Free mf (fmap ($a) . kf)
  Free mf kf <*> Free ma ka =
    Free (liftA2 (,) mf ma) $ \(x,y) -> kf x <*> ka y

instance Commutative m => Monad (Free m) where
  Pure a >>= f = f a
  Free ma ka >>= f = Free ma (ka >=> f)

-- run, using commutativity to sort as much stuff as possible before the binds
eval :: Monad m => Free m a -> m a
eval (Pure a) = pure a
eval (Free m k) = m >>= eval . k

newtype Prob a = Prob { runProb :: [(a, Double)] }
  deriving (Functor, Show)

instance Applicative Prob where
  pure a = Prob [(a,1)]
  (<*>) = ap

instance Monad Prob where
  Prob as >>= f = Prob
     [ (b,p*q) | (a,p) <- as, (b,q) <- runProb (f a) ]

instance Commutative Prob

collapse :: Ord a => Prob a -> Map a Double
collapse = fromListWith (+) . runProb

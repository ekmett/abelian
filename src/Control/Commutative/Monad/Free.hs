{-# language GADTs #-}
{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module Control.Commutative.Monad.Free
  ( Free(..)
  , eval
  ) where

import Control.Applicative (liftA2)
import Control.Commutative
import Control.Monad.Free.Class

data Free m a = Pure a | Free (m (Free m a))
  deriving Functor

instance Commutative m => Applicative (Free m) where
  pure = Pure
  Pure f <*> ma = fmap f ma
  Free m <*> Pure a = Free (fmap ($a) <$> m)
  Free mf <*> Free ma = Free $ liftA2 (<*>) mf ma

instance Commutative m => Monad (Free m) where
  Pure a >>= f = f a
  Free ma >>= f = Free $ (>>= f) <$> ma

instance Commutative m => MonadFree m (Free m) where
  wrap = Free

eval :: Monad m => Free m a -> m a
eval (Pure a) = pure a
eval (Free m) = m >>= eval

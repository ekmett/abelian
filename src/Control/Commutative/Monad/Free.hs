{-# language GADTs #-}
{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language DeriveGeneric #-}
{-# language DeriveTraversable #-}

module Control.Commutative.Monad.Free
  ( Free(..)
  , eval
  ) where

import Control.Applicative (liftA2)
import Control.Commutative
import Control.Monad.Free.Class
import Data.Functor.Bind
import Data.Functor.Comm
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import GHC.Generics

-- | The free monad given a commutative applicative functor.
data Free m a = Pure a | Free (m (Free m a))
  deriving (Functor, Foldable, Traversable, Generic, Generic1)

instance Foldable1 m => Foldable1 (Free m) where
  foldMap1 f (Pure a) = f a
  foldMap1 f (Free ma) = foldMap1 (foldMap1 f) ma

instance Traversable1 m => Traversable1 (Free m) where
  traverse1 f (Pure a) = Pure <$> f a
  traverse1 f (Free ma) = Free <$> traverse1 (traverse1 f) ma

instance Comm m => Apply (Free m) where
  Pure f <.> Pure a = Pure $ f a
  Pure f <.> Free ma = Free $ fmap f <$> ma
  Free m <.> Pure a = Free $ fmap ($a) <$> m
  Free mf <.> Free ma = Free $ liftF2 (<.>) mf ma

instance Commutative m => Applicative (Free m) where
  pure = Pure
  Pure f <*> Pure a = Pure $ f a
  Pure f <*> Free ma = Free $ fmap f <$> ma
  Free m <*> Pure a = Free $ fmap ($a) <$> m
  Free mf <*> Free ma = Free $ liftA2 (<*>) mf ma

instance Comm m => Comm (Free m)

instance Commutative m => Commutative (Free m)

instance Comm m => Bind (Free m) where
  Pure a >>- f = f a
  Free ma >>- f = Free $ (>>- f) <$> ma

instance Commutative m => Monad (Free m) where
  Pure a >>= f = f a
  Free ma >>= f = Free $ (>>= f) <$> ma

instance Commutative m => MonadFree m (Free m) where
  wrap = Free

eval :: Monad m => Free m a -> m a
eval (Pure a) = pure a
eval (Free m) = m >>= eval

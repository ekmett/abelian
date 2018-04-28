{-# language DeriveTraversable #-}
module Control.Commutative.Monad.List where

newtype ListT m a = ListT { runListT :: m [a] }
  deriving (Functor, Foldable, Traversable)

instance Applicative m => Applicative (ListT m) where
  pure = ListT . pure . pure
  ListT mf <*> ListT ma = ListT $ (<*>) <$> mf <*> ma

--  instance CommutativeMonad m => Monad (ListT m) where
--   ListT m >>= 

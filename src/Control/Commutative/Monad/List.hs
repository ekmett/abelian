{-# language DeriveTraversable #-}
{-# language MonoLocalBinds #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language UndecidableInstances #-}
module Control.Commutative.Monad.List 
  ( ListT(..)
  , mapListT
  ) where

import Control.Applicative
import Control.Commutative
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Reader.Class
import Data.Functor.Classes

liftListT :: Functor m => m a -> ListT m a
liftListT = ListT . fmap pure 

newtype ListT m a = ListT { runListT :: m [a] }
  deriving (Functor, Foldable, Traversable)

instance Eq1 m => Eq1 (ListT m) where
  liftEq f (ListT m) (ListT n) = liftEq (liftEq f) m n

instance Ord1 m => Ord1 (ListT m) where
  liftCompare f (ListT m) (ListT n) = liftCompare (liftCompare f) m n

instance Show1 m => Show1 (ListT m) where
  liftShowsPrec s sl i (ListT m) = 
    showParen (i > 10) $ showString "ListT " . 
       liftShowsPrec (const sl) (liftShowList s sl) 11 m

instance Read1 m => Read1 (ListT m) where
  liftReadsPrec rp rl = readsData $
      readsUnaryWith (liftReadsPrec rp' rl') "ListT" ListT
    where
      rp' = liftReadsPrec rp rl
      rl' = liftReadList rp rl

instance (Eq1 m, Eq a) => Eq (ListT m a) where
  (==) = liftEq (==)

instance (Ord1 m, Ord a) => Ord (ListT m a) where
  compare = liftCompare compare

instance (Show1 m, Show a) => Show (ListT m a) where
  showsPrec = liftShowsPrec showsPrec showList

instance (Read1 m, Read a) => Read (ListT m a) where
  readsPrec = liftReadsPrec readsPrec readList
      
instance Applicative m => Applicative (ListT m) where
  pure = ListT . pure . pure
  ListT mf <*> ListT ma = ListT $ (<*>) <$> mf <*> ma

instance Applicative m => Alternative (ListT m) where
  empty = ListT $ pure []
  m <|> n = ListT $ (++) <$> runListT m <*> runListT n

instance CommutativeMonad m => Monad (ListT m) where
  m >>= k = ListT $ do
    a <- runListT m
    b <- traverse (runListT . k) a
    return (concat b)

instance CommutativeMonad m => MonadFail (ListT m) where
  fail _ = ListT $ return []

instance CommutativeMonad m => MonadPlus (ListT m) where
  mzero = ListT $ return []
  m `mplus` n = ListT $ do
    a <- runListT m
    b <- runListT n
    return (a ++ b)

instance (MonadReader e m, CommutativeMonad m) => MonadReader e (ListT m) where
  ask = liftListT ask
  local f = mapListT (local f)
  reader = liftListT . reader

mapListT :: (m [a] -> n [b]) -> ListT m a -> ListT n b
mapListT f m = ListT $ f (runListT m)

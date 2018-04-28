{-# language DefaultSignatures #-}
{-# language MonoLocalBinds #-}

module Control.Commutative.Monad.Trans where

import Control.Commutative
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Identity
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import Data.Semigroup.Commutative

-- this also morally implies CommutativeMonad m => CommutativeMonad (t m)
-- which unfortunately rules out ListT
class CommutativeMonadTrans t where
  liftCommutative :: CommutativeMonad m => m a -> t m a
  default liftCommutative :: (MonadTrans t, CommutativeMonad m) => m a -> t m a
  liftCommutative = lift

instance CommutativeMonadTrans (ReaderT e)
instance CommutativeMonoid w => CommutativeMonadTrans (Strict.WriterT w)
instance CommutativeMonoid w => CommutativeMonadTrans (Lazy.WriterT w)
instance CommutativeMonadTrans IdentityT

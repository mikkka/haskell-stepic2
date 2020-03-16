{-# LANGUAGE InstanceSigs #-}
module ReaderT where

import Control.Applicative
import Control.Monad
import MonadTrans
import Data.Functor.Identity

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

reader :: Monad m => (r -> a) -> ReaderT r m a
reader f = ReaderT $ return . f 

instance Functor m => Functor (ReaderT r m) where
  fmap f a = ReaderT $ fmap f . runReaderT a

instance Applicative m => Applicative (ReaderT r m) where
  pure    = ReaderT . const . pure
  -- f <*> v = ReaderT $ \env  -> runReaderT f env <*> runReaderT v env
  f <*> v = ReaderT $ liftA2 (<*>) (runReaderT f) (runReaderT v)

instance Monad m => Monad (ReaderT r m) where
  v >>= f  = ReaderT $ \env -> do
      v' <- runReaderT v env
      runReaderT (f v') env

instance MonadTrans (ReaderT t) where
  lift m = ReaderT $ const m

ask :: Monad m => ReaderT r m r
ask = ReaderT return

asks :: Monad m => (r -> a) -> ReaderT r m a
asks f = ReaderT $ return . f

local :: (r -> r) -> ReaderT r m a -> ReaderT r m a
local f rdr = ReaderT $ runReaderT rdr . f

type Reader e = ReaderT e Identity
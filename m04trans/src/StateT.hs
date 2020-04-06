module StateT where

import MonadTrans
import ReaderT
import Control.Applicative
import Data.Functor.Identity

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT (return . f)

instance Functor m => Functor (StateT s m) where
  -- fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f m = StateT $ \st -> fmap updater $ runStateT m st
    where updater ~(x, s) = (f x, s)

instance Monad m => Applicative (StateT s m) where
  -- pure :: a -> StateT s m a
  pure x = StateT $ \ s -> return (x, s)

  -- (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  f <*> v = StateT $ \ s -> do
      ~(g, s') <- runStateT f s
      ~(x, s'') <- runStateT v s'
      return (g x, s'')

instance Monad m => Monad (StateT s m) where
  -- (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  m >>= k  = StateT $ \s -> do
    ~(x, s') <- runStateT m s
    runStateT (k x) s'

  fail = StateT . const . fail 


instance MonadTrans (StateT s) where
  -- lift :: Monad m => m a -> StateT s m a
  lift m = StateT $ \st -> do
    a <- m
    return (a, st)


get :: Monad m => StateT s m s
get = state $ \s -> (s, s)

put :: Monad m => s -> StateT s m ()
put s = state $ const ((), s)

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = state $ \s -> ((), f s)

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT st s = fst <$> runStateT st s

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT st s = snd <$> runStateT st s

readerToStateT :: Monad m => ReaderT r m a -> StateT r m a
readerToStateT rdr = StateT $ \s -> (\a -> (a,s)) <$> runReaderT rdr s

type State s = StateT s Identity

runState m s = runIdentity $ runStateT m s
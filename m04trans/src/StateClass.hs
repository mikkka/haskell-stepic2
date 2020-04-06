{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module StateClass where

import MonadTrans
import qualified StateT

import LoggT
import StateT(State, runState)


class (Monad m) => MonadState s m | m -> s where
  get   :: m s
  put   :: s -> m ()
  state :: (s -> (a, s)) -> m a

modify :: MonadState s m => (s -> s) -> m ()
modify f = state $ \s -> ((), f s)

instance (Monad m) => MonadState s (StateT.StateT s m) where
  get   = StateT.get
  put   = StateT.put
  state = StateT.state

instance MonadState s m => MonadState s (LoggT m) where
  -- get   = LoggT $ Logged "" <$> get
  get   = lift get
  put   = lift . put
  state = lift . state

logSt' :: LoggT (State Integer) Integer
logSt' = do
  modify (+1)                   -- no lift!
  a <- get                      -- no lift!
  write2log $ show $ a * 10
  put 42                        -- no lift!
  return $ a * 100

test = runState (runLoggT logSt') 2
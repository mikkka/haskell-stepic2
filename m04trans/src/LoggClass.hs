{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module LoggClass where

import MonadTrans
import LoggT
import StateT
import ReaderT

class Monad m => MonadLogg m where
  w2log :: String -> m ()
  logg :: Logged a -> m a

instance Monad m => MonadLogg (LoggT m) where
  w2log = write2log
  logg  = LoggT . return

instance MonadLogg m => MonadLogg (StateT s m) where
  w2log = lift . w2log
  logg  = lift . logg

instance MonadLogg m => MonadLogg (ReaderT r m) where
  w2log = lift . w2log
  logg  = lift . logg
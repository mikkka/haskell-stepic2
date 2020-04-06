{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module ReaderClass where

import MonadTrans
import qualified ReaderT

import LoggT
import ReaderT(Reader, runReader)

class (Monad m) => MonadReader r m | m -> r where
  ask    :: m r
  local  :: (r -> r) -> m a -> m a
  reader :: (r -> a) -> m a

asks :: MonadReader r m => (r -> a) -> m a
asks f = reader $ \r -> f r 

instance (Monad m) => MonadReader r (ReaderT.ReaderT r m) where
  ask    = ReaderT.ask 
  local  = ReaderT.local
  reader = ReaderT.reader

instance MonadReader r m => MonadReader r (LoggT m) where
  ask       = lift ask
  local f g = LoggT $ local f $ runLoggT g -- can be simplified to next by reducing g and replacing $ with .
  -- local f   = LoggT . local f . runLoggT 
  reader    = lift . reader

logRdr :: LoggT (Reader [(Int,String)]) ()      
logRdr = do 
  x <- asks $ lookup 2                      -- no lift!
  write2log (maybe "Nothing" id x)
  y <- local ((3,"Jim"):) $ asks $ lookup 3 -- no lift!
  write2log (maybe "Nothing" id y)

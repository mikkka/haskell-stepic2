module ExceptT where

import Control.Applicative
import MonadTrans
import Data.Functor.Identity

newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

type Except e a = ExceptT e Identity a

runExcept :: Except e a -> Either e a
runExcept = runIdentity . runExceptT

except :: Monad m => Either e a -> ExceptT e m a
except  = ExceptT . return

instance Functor m => Functor (ExceptT e m) where
  fmap f = ExceptT . fmap (fmap f) . runExceptT

instance Applicative m => Applicative (ExceptT e m) where
  pure    = ExceptT . pure . Right
  f <*> v = ExceptT $ liftA2 (<*>) (runExceptT f) (runExceptT v)

instance Monad m => Monad (ExceptT e m ) where
  return   = pure

  m >>= k  = ExceptT $ do
    a <- runExceptT m 
    case a of
      Left e  -> return $ Left e
      Right v -> runExceptT (k v)

  fail = ExceptT . fail 

instance MonadTrans (ExceptT e) where
  lift = ExceptT . fmap Right

throwE :: Monad m => e -> ExceptT e m a
throwE = ExceptT . return . Left 

catchE :: Monad m => ExceptT e m a -> (e -> ExceptT e' m a) -> ExceptT e' m a
m `catchE` h = ExceptT $ do
  a <- runExceptT m
  case a of 
    Left l -> runExceptT $ h l
    Right r -> return $ Right r

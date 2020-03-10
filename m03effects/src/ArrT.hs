{-# LANGUAGE InstanceSigs #-}
module ArrT where

import Control.Applicative

newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }

arr2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
arr2 f = Arr2T (\e1 e2 -> return $ f e1 e2)

arr3 :: Monad m => (e1 -> e2 -> e3 -> a) -> Arr3T e1 e2 e3 m a
arr3 f = Arr3T (\e1 e2 e3 -> return $ f e1 e2 e3)

instance Functor m => Functor (Arr2T e1 e2 m) where
  fmap f a = Arr2T (\e1 e2 -> fmap f $ getArr2T a e1 e2) 

instance Functor m => Functor (Arr3T e1 e2 e3 m) where
  fmap f a = Arr3T (\e1 e2 e3 -> fmap f $ getArr3T a e1 e2 e3) 

instance Applicative m => Applicative (Arr2T e1 e2 m) where
  pure     = Arr2T . const . const . pure
  f <*> v  = Arr2T $ (liftA2 $ liftA2 (<*>)) (getArr2T f) (getArr2T v)

instance Applicative m => Applicative (Arr3T e1 e2 e3 m) where
  pure     = Arr3T . const . const . const . pure
  f <*> v  = Arr3T $ (liftA2 $ liftA2 $ liftA2 (<*>)) (getArr3T f) (getArr3T v)

instance Monad m => Monad (Arr2T e1 e2 m) where
  f >>= v  = Arr2T $ \e1 e2 -> do 
    f' <- getArr2T f e1 e2
    getArr2T (v f') e1 e2
  
  fail err = Arr2T $ \e1 e2 -> fail err

instance Monad m => Monad (Arr3T e1 e2 e3 m) where
  f >>= v  = Arr3T $ \e1 e2 e3 -> do 
    f' <- getArr3T f e1 e2 e3
    getArr3T (v f') e1 e2 e3

  fail err = Arr3T $ \e1 e2 e3 -> fail err

class MonadTrans t where
  lift :: Monad m => m a -> t m a

instance MonadTrans (Arr2T e1 e2) where
  lift m = Arr2T $ (const . const) m

asks2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
asks2 f = Arr2T $ \e1 -> return . f e1  
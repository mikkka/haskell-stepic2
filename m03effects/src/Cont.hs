module Cont where

import Control.Monad (liftM, ap)

newtype Cont r a = Cont {runCont :: (a -> r) -> r}

instance Functor (Cont r) where
  fmap = liftM

instance Applicative (Cont r) where
  pure = return
  (<*>) = ap

instance Monad (Cont r) where
  -- return :: a -> Cont r a
  return x      = Cont $ \c -> c x
  -- (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
  Cont v >>= k  = Cont $ \c -> v (\a -> runCont (k a) c)

-- wo Cont

bind :: ((a -> r) -> r) -> (a -> (b -> r) -> r) -> ((b -> r) -> r)
bind v k = \c -> v (\a -> k a c)


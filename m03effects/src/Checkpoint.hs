module Checkpoint where

newtype Cont r a = Cont {runCont :: (a -> r) -> r}

instance Functor (Cont r) where
  fmap f x = (pure f) <*> x

instance Applicative (Cont r) where
  pure = return
  af <*> av = do
    f <- af
    v <- av
    return (f v)

instance Monad (Cont r) where
  -- return :: a -> Cont r a
  return x = Cont $ \c -> c x

  -- (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
  Cont v >>= k = Cont $ \c -> v (\a -> runCont (k a) c)

type Checkpointed r = (r -> Cont r Bool) -> Cont r r
-- type Checkpointed a = (a -> Cont a Int) -> Cont a a

runCheckpointed :: (a -> Bool) -> Checkpointed a -> a
runCheckpointed pred check = undefined
  
addTens :: Int -> Checkpointed Int
addTens x1 = \checkpoint -> do
  checkpoint x1
  let x2 = x1 + 10
  checkpoint x2     {- x2 = x1 + 10 -}
  let x3 = x2 + 10
  checkpoint x3     {- x3 = x1 + 20 -}
  let x4 = x3 + 10
  return x4         {- x4 = x1 + 30 -}

res31 :: Int
res31 = runCheckpointed (< 100) $ addTens 1
-- res21 = runCheckpointed  (< 30) $ addTens 1
-- res11 = runCheckpointed  (< 20) $ addTens 1
-- res01 = runCheckpointed  (< 10) $ addTens 1


sumIt :: Cont Int Int
sumIt = do
  a <- return 3
  -- b <- Cont $ \c -> 1488
  b <- return 8 
  return $ a + b
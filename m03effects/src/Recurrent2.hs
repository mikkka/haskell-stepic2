module Recurrent2 where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

tickCollatz' :: StateT Integer IO Integer
tickCollatz' = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  lift $ putStrLn $ show res
  put res
  return n

type RiiEsSiT m = ReaderT (Integer,Integer) (ExceptT String (StateT Integer m))

runRiiEsSiT :: ReaderT (Integer,Integer) (ExceptT String (StateT Integer m)) a 
                -> (Integer,Integer)  
                -> Integer 
                -> m (Either String a, Integer)
runRiiEsSiT calc minMax = runStateT $ runExceptT $ runReaderT calc minMax

go :: Monad m => StateT Integer m Integer -> RiiEsSiT m ()
go step = do 
  (min, max) <- ask 
  lift $ lift step
  n <- lift $ lift get
  when (n <= min) $ lift $ throwE "Lower bound"
  when (n >= max) $ lift $ throwE "Upper bound"
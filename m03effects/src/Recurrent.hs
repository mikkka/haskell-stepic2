module Recurrent where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Functor

tickCollatz :: State Integer Integer
tickCollatz = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  put res
  return n

type EsSi = ExceptT String (State Integer)

runEsSi :: EsSi a -> Integer -> (Either String a, Integer)
runEsSi essi = runState $ runExceptT essi   

go :: Integer -> Integer -> State Integer Integer -> EsSi ()
go min max state = do 
    lift state
    n <- lift get
    when (n <= min) $ throwE "Lower bound"
    when (n >= max) $ throwE "Upper bound"



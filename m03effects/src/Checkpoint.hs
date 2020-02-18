module Checkpoint where

import Cont

addTens :: Int -> Checkpointed Int
addTens x1 = \checkpoint -> do
  checkpoint x1
  let x2 = x1 + 10
  checkpoint x2     {- x2 = x1 + 10 -}
  let x3 = x2 + 10
  checkpoint x3     {- x3 = x1 + 20 -}
  let x4 = x3 + 10
  return x4         {- x4 = x1 + 30 -}

type Checkpointed a = (a -> Cont a a) -> Cont a a

runCheckpointed :: (a -> Bool) -> Checkpointed a -> a
runCheckpointed pred check = runCont (check newCont) id where 
  newCont x = Cont $ \c -> let y = c x in 
    if not $ pred y then x else y

-- key thought to the solution
addTensS :: Int -> (Int -> Bool) -> Cont Int Int
addTensS x1 pred = do
  Cont $ (\c -> if not (pred (c x1)) then x1 else c x1)
  let x2 = x1 + 10
  Cont $ (\c -> if not (pred (c x2)) then x2 else c x2)
  let x3 = x2 + 10
  Cont $ (\c -> if not (pred (c x3)) then x3 else c x3)
  let x4 = x3 + 10
  return x4
module Tree where

import StateT
import WriterT
import Data.Monoid
import MonadTrans

data Tree a = Leaf a | Fork (Tree a) a (Tree a)

numberAndCount :: Tree () -> (Tree Integer, Integer)
numberAndCount t = getSum <$> runWriter (evalStateT (go t) 1)
  where
    go :: Tree () -> StateT Integer (Writer (Sum Integer)) (Tree Integer)
    go (Leaf _) = do
                  counter <- get
                  modify ( + 1)
                  lift $ tell 1
                  return $ Leaf counter
    go (Fork lft _ rgt) = do
                  lft' <- go lft
                  counter <- get
                  modify ( + 1)
                  rgt' <- go rgt
                  return $ Fork lft' counter rgt'

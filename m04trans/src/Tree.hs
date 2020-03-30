module Tree where

import StateT
import WriterT
import Data.Monoid
import MonadTrans
import ExceptT
import TryRead
import Data.Foldable
import Control.Monad

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

instance Foldable Tree where
  foldr f ini (Leaf a) = f a ini
  foldr f ini (Fork l x r) = foldr f (f x (foldr f ini r)) l

treeSum :: Tree String -> (Maybe ReadError, Integer)
treeSum t = let (err, s) = runWriter . runExceptT $ traverse_ go t
            in (maybeErr err, getSum s)
  where
    maybeErr :: Either ReadError () -> Maybe ReadError
    maybeErr = either Just (const Nothing)

go :: String -> ExceptT ReadError (Writer (Sum Integer)) ()
-- go s = do 
--   x <- tryRead s
--   lift $ tell (Sum x)
go = tryRead >=> lift . tell . Sum
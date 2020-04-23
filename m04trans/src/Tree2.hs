{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Tree2 where

import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad
import Data.Foldable

import TryRead2

data Tree a = Leaf a | Fork (Tree a) a (Tree a)

instance Foldable Tree where
  foldr f ini (Leaf a) = f a ini
  foldr f ini (Fork l x r) = foldr f (f x (foldr f ini r)) l

treeSum :: Tree String -> Either ReadError Integer
treeSum t = getSum <$> execWriterT (traverse_ (tryRead >=> tell . Sum) t)
-- the next one from forum as example of simple solution
-- treeSum (Leaf s) = tryRead s
-- treeSum (Fork l a r) = do
--   ln <- treeSum l
--   n  <- tryRead a
--   rn <- treeSum r
--   return (ln+n+rn)
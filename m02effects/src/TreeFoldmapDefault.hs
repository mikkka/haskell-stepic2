module TreeFoldmapDefault where

import Data.Traversable (foldMapDefault)

data Tree a = Nil | Branch (Tree a) a (Tree a)  deriving (Eq, Show) 

instance Foldable Tree where
  foldMap = foldMapDefault

instance Traversable Tree where
  sequenceA Nil = pure Nil
  sequenceA (Branch lft x rgt) = (flipL Branch) <$> (sequenceA lft) <*> (sequenceA rgt) <*> x
    where 
      flipL f a b c = f a c b

instance Functor Tree where
  fmap _ Nil = Nil
  fmap f (Branch l x r) = Branch (f <$> l) (f x) (f <$> r)

flip2 :: (a -> b -> c -> d) -> a -> c -> b -> d
flip2 f a b c = f a c b

testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)

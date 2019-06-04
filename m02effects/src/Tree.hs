module Tree where

data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)

newtype Preorder a   = PreO   (Tree a)    deriving (Eq, Show)
newtype Postorder a  = PostO  (Tree a)    deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a)    deriving (Eq, Show)

instance Foldable Tree where
  foldr _ ini Nil = ini
  foldr f ini (Branch l x r) = foldr f (f x (foldr f ini r)) l

instance Functor Tree where
  fmap _ Nil = Nil
  fmap f (Branch l x r) = Branch (f <$> l) (f x) (f <$> r)

instance Traversable Tree where
  traverse _ Nil = pure Nil
  traverse f (Branch l x r) = Branch <$> traverse f l <*> f x <*> traverse f r

instance Foldable Preorder where
  foldr _ ini (PreO Nil) = ini
  foldr f ini (PreO (Branch l x r)) = f x (foldr f (foldr f ini (PreO r)) (PreO l))

instance Foldable Postorder where
  foldr _ ini (PostO Nil) = ini
  foldr f ini (PostO (Branch l x r)) =  (\i -> (foldr f i (PostO l))) . (\i -> (foldr f i (PostO r))) . (f x) $ ini 

instance Foldable Levelorder where
  foldr f ini (LevelO tree) = foldr f ini (toList [tree])
    where
      toList :: [Tree a] -> [a]
      toList [] = []
      toList xs = concatMap treeValue xs ++ toList (concatMap treeChildren xs)

      treeValue Nil                     =  []
      treeValue (Branch left x right)   =  [x]

      treeChildren Nil                  = []
      treeChildren (Branch Nil _ Nil)   = []
      treeChildren (Branch Nil _ rgt)   = [rgt]
      treeChildren (Branch lft _ Nil)   = [lft]
      treeChildren (Branch lft _ rgt)   = [lft,rgt]


module OddC where

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

instance Functor OddC where
  fmap f (Un a) = Un $ f a 
  fmap f (Bi a1 a2 o) = Bi (f a1) (f a2) (f <$> o)

instance Foldable OddC where
  foldr f ini (Un a) = f a ini
  foldr f ini (Bi a1 a2 o) = f a1 (f a2 (foldr f ini o))

instance Traversable OddC where
  traverse f (Un a) = Un <$> f a
  traverse f (Bi a1 a2 o) = Bi <$> f a1 <*> f a2 <*> traverse f o 

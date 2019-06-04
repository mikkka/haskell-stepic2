module Triple where

data Triple a = Tr a a a  deriving (Eq,Show)

instance Functor Triple where
  fmap f (Tr x y z) = Tr (f x) (f y) (f z)

instance Foldable Triple where
  foldr f x (Tr x1 x2 x3) = f x1 (f x2 (f x3 x))
  foldl f x (Tr x1 x2 x3) = f (f (f x x1) x2) x3

instance Traversable Triple where 
  traverse f (Tr x y z) = Tr <$> f x <*> f y <*> f z
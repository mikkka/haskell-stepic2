module ErrorOrOk where

data Result a = Ok a | Error String deriving (Eq,Show)

instance Functor Result where
  fmap f (Ok x) = Ok (f x) 
  fmap f (Error str) = Error str 

instance Foldable Result where
  foldr f x (Ok x1) = f x1 x 
  foldr f x (Error str) = x 

instance Traversable Result where 
  traverse f (Ok x) = Ok <$> f x 
  traverse f (Error str) = pure (Error str) 
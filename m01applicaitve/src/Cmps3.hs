module Cmps3 where

newtype Cmps3 f g h a = Cmps3 { getCmps3 :: f (g (h a)) } 
  deriving (Eq,Show) 

instance (Functor f, Functor g, Functor h) => Functor (Cmps3 f g h) where
  fmap ff (Cmps3 x) = Cmps3 $ fmap (fmap (fmap ff)) x
module Traverse2list where

traverse2list :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]
traverse2list f = foldr (\x y -> pure (:) <*> f x <*> y) (pure [])
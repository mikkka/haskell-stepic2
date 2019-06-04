{-# LANGUAGE TypeOperators #-}
module CmpsFoldable where
import Data.Monoid

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show) 

instance (Functor f, Functor g) => Functor (f |.| g) where
  fmap f (Cmps ta) = Cmps (fmap (fmap f) $ ta)

instance (Foldable f, Foldable g) => Foldable (f |.| g) where
  foldMap f (Cmps ta) = foldMap (foldMap f) $ ta

instance (Traversable f, Traversable g) => Traversable (f |.| g) where
  traverse f (Cmps ta) = Cmps <$> (traverse (traverse f) $ ta) 


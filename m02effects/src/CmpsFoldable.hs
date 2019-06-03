{-# LANGUAGE TypeOperators #-}
module CmpsFoldable where
import Data.Monoid

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show) 

instance (Foldable f, Foldable g) => Foldable (f |.| g) where
  foldMap f (Cmps ta) = foldMap (foldMap f) $ ta
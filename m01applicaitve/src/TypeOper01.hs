{-# LANGUAGE TypeOperators #-}
module TypeOper01 where

infixr 9 |.|
newtype (|.|) f g a = Cmps {getCmps :: f (g a)} deriving (Eq, Show)


type A   = ((,) Integer |.| (,) Char) Bool
type B t = ((,,) Bool (t -> t) |.| Either String) Int
type C   = (|.|) ((->) Bool) ((->) Integer) Integer

a :: A
a = Cmps (42, ('A', True))

b :: B t
b = Cmps (True, id, Left "42")

c :: C
c  = Cmps (\b i -> 42)

unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
unCmps3 = fmap getCmps . getCmps

unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
unCmps4 = fmap unCmps3 . getCmps 
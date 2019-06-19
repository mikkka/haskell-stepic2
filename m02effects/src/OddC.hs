module OddC where

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

instance Foldable OddC where
  foldr f ini (Un a) = f a ini
  foldr f ini (Bi a1 a2 o) = f a1 (f a2 (foldr f ini o))

instance Traversable OddC where
  traverse f (Un a) = Un <$> f a
  traverse f (Bi a1 a2 o) = Bi <$> f a1 <*> f a2 <*> traverse f o 

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un a1) (Un a2) a3  = Bi a1 a2 a3 
concat3OC (Un a1) (Bi a2 a3 odd) x = Bi a1 a2 $ concat3OC (Un a3) odd x
concat3OC (Bi a1 a2 odd) x y = Bi a1 a2 $ concat3OC odd x y

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un x) = x
concatOC (Bi a1 a2 xs) = concat3OC a1 a2 (concatOC xs)

instance Functor OddC where
  fmap f (Un a) = Un $ f a 
  fmap f (Bi a1 a2 o) = Bi (f a1) (f a2) (f <$> o)

instance Applicative OddC where
  pure a = Un a
  (<*>) fs xs = do {f <- fs; x <- xs; return $ f x} 

instance Monad OddC where
  return a = Un a
  (>>=) fa f = concatOC (fmap f fa)
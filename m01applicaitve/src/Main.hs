module Main where

main :: IO ()
main = do
  putStrLn "hello world"

{-
(Char -> Char) -> [Char] -> [Char]

(a -> b) -> (e -> a) -> (e -> b)

(Char -> Char) -> (Char -> Char) -> (Char -> Char)
-}

{-
база
fmap f (fmap g []) = fmap (f . g) [] 

предположение индукции
fmap f (fmap g xs) = fmap (f . g) xs

шаг индукции
fmap f (fmap g (x : xs) = fmap (f . g) (x : xs)

fmap f ((g x) : fmap g xs) = ((f . g) x) : (fmap (f . g) (xs))

(f (g x)) : fmap f (fmap g xs) = ((f . g) x) : (fmap (f . g) (xs))

((f . g) x) : fmap f (fmap g xs) = ((f . g) x) : (fmap (f . g) (xs))
головы и хвосты списков с обоих сторон равны
-}
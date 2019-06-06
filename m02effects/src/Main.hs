module Main where

main :: IO ()
main = putStrLn "hello world"



{-
sequenceA :: Applicative f => t (f a) -> f (t a)

(Applicative f, Applicative g, Traversable t)

t (f (g a)) -> Compose f g (t a)

Compose . fmap sequenceA . sequenceA

fmap sequenceA . sequenceA == t (f (g a)) -> f g (t a) 


sequenceA == t (f (g a)) -> f (t (g a)) 
-}
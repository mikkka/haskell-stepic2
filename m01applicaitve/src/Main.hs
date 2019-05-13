module Main where

main :: IO ()
main = do
  putStrLn "hello world"

{-
(Char -> Char) -> [Char] -> [Char]

(a -> b) -> (e -> a) -> (e -> b)

(Char -> Char) -> (Char -> Char) -> (Char -> Char)
-}
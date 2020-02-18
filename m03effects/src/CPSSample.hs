{-# LANGUAGE InstanceSigs #-}
module CPSSample where
import Control.Monad (when)

square :: Int -> (Int -> r) -> r
square x c = c $ x ^ 2

add :: Int -> Int -> (Int -> r) -> r
add x y c = c $ x + y

sumSquares :: Int -> Int -> (Int -> r) -> r
sumSquares x y c = 
  square x  $ \x2 -> 
  square y  $ \y2 -> 
  add x2 y2 $ \res -> 
  c res


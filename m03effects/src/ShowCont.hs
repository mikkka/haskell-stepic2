module ShowCont where 

import Cont

evalCont :: Cont r r -> r
evalCont m = runCont m id

showCont :: Show a => Cont String a -> String
showCont m = runCont m show

square' :: Int -> Cont r Int
square' x = Cont $ \c -> c $ x ^ 2 

add' :: Int -> Int -> Cont r Int
add' x y = Cont $ \c -> c $ x + y

sumSquares' :: Int -> Int -> Cont r Int
sumSquares' x y = do
  x1 <- square' x
  y1 <- square' y
  add' x1 y1

sumIt :: Cont String Integer
sumIt = do
  a <- return 3
  b <- Cont $ \c -> "KEK"
  return $ a + b

sumItS :: Cont [r] Integer
sumItS = do
  a <- return 3
  b <- Cont $ \c -> c 4 ++ c 5
  return $ a + b

-- let kek = runCont sumItS show
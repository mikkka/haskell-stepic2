module FailCont where

import ReadExcept
import Control.Monad (liftM, ap)
import Control.Monad.Trans.Except

add :: Int -> Int -> FailCont r e Int
add x y = FailCont $ \ok err -> ok $ x + y

addInts :: String -> String -> FailCont r ReadError Int
addInts s1 s2 = do
  i1 <- toFailCont $ tryRead s1
  i2 <- toFailCont $ tryRead s2
  return $ i1 + i2

newtype FailCont r e a = FailCont {
  runFailCont :: (a -> r) -> (e -> r) -> r
}

instance Functor (FailCont r e) where
  fmap = liftM

instance Applicative (FailCont r e) where
  pure = return
  (<*>) = ap

instance Monad (FailCont r e) where
  return x          = FailCont $ \ok err -> ok x
  FailCont v >>= k  = FailCont $ \onOk onErr -> 
    v (\ok -> runFailCont (k ok) onOk onErr) onErr

toFailCont :: Except e a -> FailCont r e a
toFailCont ex = FailCont $ \ok err -> 
  case runExcept ex of
    Left e -> err e
    Right r -> ok r

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont cont = runFailCont cont Right Left

callCFC :: ((a -> FailCont r e b) -> FailCont r e a) -> FailCont r e a
callCFC f = FailCont $ 
            \ok err -> runFailCont (f $ \a -> FailCont $ \_ _ -> ok a) ok err

--------- compose on functions --------------------

sumInt :: Int -> Int -> (Int -> r) -> (String -> r) -> r
sumInt x y ok err = ok $ x + y 
  
divFour :: Int -> (Int -> r) -> (String -> r) -> r
divFour x ok err =  
  if x == 0 then err "/0" else ok (4 `div` x)

-- divFour x -> divFour x -> sum
divAndAdd :: Int -> Int -> (Int -> r) -> (String -> r) -> r
divAndAdd x y = (\onOk onErr -> 
  divFour x (\ok1 -> 
    divFour y (\ok2 -> 
      sumInt ok1 ok2 (\ok3 -> onOk ok3) (\err3 -> onErr err3)
    ) 
    (\err2 -> onErr err2)
  ) 
  (\err1 -> onErr err1))
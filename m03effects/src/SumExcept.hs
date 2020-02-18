module SumExcept where

import ReadExcept
import Control.Monad.Trans.Except

data SumError = SumError Int ReadError
  deriving Show

trySum :: [String] -> Except SumError Integer
trySum strs = sum strs 1
  where
    sum [] _        = return 0
    sum (x: xs) idx =
      do 
        val <- withExcept (\e -> SumError idx e) (tryRead x)
        rest <- sum xs (idx + 1)
        return (val + rest)

trySum2 :: [String] -> Except SumError Integer
trySum2 xs = sum <$> traverse (\(i, s) -> withExcept (SumError i) $ tryRead s) (zip [1..] xs)


newtype Validate e a = Validate {getValidate :: Either [e] a}
instance Functor (Validate e) where
  fmap f x = f <$> x

instance Applicative (Validate e) where
  pure = Validate . Right
  (<*>) (Validate (Left e1)) (Validate (Left e2)) = Validate $ Left (e1 ++ e2)
  (<*>) (Validate (Right _)) (Validate (Left e2)) = Validate $ Left e2
  (<*>) (Validate (Left e1)) (Validate (Right _)) = Validate $ Left e1
  (<*>) (Validate (Right f)) (Validate (Right x)) = Validate $ Right (f x)

collectE :: Except e a -> Validate e a
collectE = Validate . either (\a -> Left [a]) Right . runExcept

validateSum :: [String] -> Validate SumError Integer
validateSum xs = sum <$> traverse (\(i, s) -> collectE (withExcept (SumError i) $ tryRead s)) (zip [1..] xs)

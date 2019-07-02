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
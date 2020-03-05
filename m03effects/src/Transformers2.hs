module Transformers2 where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans (lift)
import Data.Char (toUpper)
import Data.List

logFirstAndRetSecond :: MyRWT IO String
logFirstAndRetSecond = do
  el1 <- myAsks head
  myLift $ putStrLn $ "First is " ++ show el1
  el2 <- myAsks (map toUpper . head . tail)
  myLift $ putStrLn $ "Second is " ++ show el2
  myTell el1
  return el2

type MyRWT m = ReaderT [String] (WriterT String m)

runMyRWT :: MyRWT m a -> [String] -> m (a, String) 
runMyRWT rwt e = runWriterT $ runReaderT rwt e 

myAsks :: Monad m => ([String] -> a) -> MyRWT m a
myAsks = asks

myAsk :: Monad m => MyRWT m [String] 
myAsk = ask

myTell :: Monad m => String -> MyRWT m ()
myTell = lift . tell

myLift :: Monad m => m a -> MyRWT m a
myLift = lift . lift

logFirstAndRetSecondSafe :: MyRWT Maybe String
logFirstAndRetSecondSafe = do
  xs <- myAsk
  case xs of
    (el1 : el2 : _) -> myTell el1 >> return (map toUpper el2)
    _ -> myLift Nothing

veryComplexComputation :: MyRWT Maybe (String, String)
veryComplexComputation = do 
  xs <- myAsk
  let (xsEven, xsOdd) = partition (even . length) xs
  case (xsEven, xsOdd) of
    ((e1 : e2 : _), (o1 : o2 : _)) -> 
      myTell (e1 ++ "," ++ o1) >> return ((map toUpper e2), (map toUpper o2))
    _ -> myLift Nothing


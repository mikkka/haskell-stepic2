module Transformers where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans (lift)
import Data.Char (toUpper)

type MyRW = ReaderT [String] (Writer String)

runMyRW :: MyRW a -> [String] -> (a, String)
runMyRW rw e = runWriter $ runReaderT rw e

myAsks :: ([String] -> a) -> MyRW a
myAsks = asks 

myTell :: String -> MyRW ()
myTell = lift . tell

logFirstAndRecord :: MyRW String
logFirstAndRecord = do
  el1 <- myAsks head
  el2 <- myAsks (map toUpper . head . tail)
  myTell el1
  return el2

type MyWR = WriterT String (Reader [String])

logFirstAndRetSecond :: MyWR String
logFirstAndRetSecond = do
  el1 <- lift $ asks head
  el2 <- lift $ asks (map toUpper . head . tail)
  tell el1
  return el2

type MyWW a = WriterT [a] (Writer [a])

separate :: (a -> Bool) -> (a -> Bool) -> [a] -> MyWW a [a]
separate p1 p2 xs = do
  tell $ filter p1 xs
  lift $ tell $ filter p2 xs
  return $ filter (\x -> not (p1 x) && not (p2 x)) xs
  

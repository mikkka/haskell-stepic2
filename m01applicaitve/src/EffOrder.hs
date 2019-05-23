{-# LANGUAGE RankNTypes#-}
module EffOrder where

import Control.Applicative ((<**>),ZipList(..))

infixl 4 <*?>
(<*?>) :: Applicative f => f a -> f (a -> b) -> f b
(<*?>) = flip (<*>)

exprMaybe :: (forall a b . Maybe a -> Maybe (a -> b) -> Maybe b) -> Maybe Int
exprMaybe op = 
  let (<??>) = op 
      infixl 4 <??> 
  in Just 5 <??> Just (+2) -- True
  -- in Nothing <??> Just (+2) -- True
  -- in Just 5 <??> Nothing -- True 
  -- in Nothing <??> Nothing -- True 

exprList :: (forall a b . [a] -> [a -> b] -> [b]) -> [Int]
exprList op = 
  let (<??>) = op 
      infixl 4 <??> 
  in [1,2,3] <??> [(+3),(+4)] -- False 

exprZipList :: (forall a b . ZipList a -> ZipList (a -> b) -> ZipList b) -> ZipList Int
exprZipList op = 
  let (<??>) = op 
      infixl 4 <??> 
  in ZipList [1,2] <??> ZipList [(+3),(+4)]  -- True 
  -- in ZipList [1,2,3] <??> ZipList [(+3),(+4)]  -- True
  -- in ZipList [1,2] <??> ZipList [(+3),(+4),(+5)]  -- True

exprEither :: (forall a b . Either String a -> Either String (a -> b) -> Either String b) -> Either String Int
exprEither op = 
  let (<??>) = op 
      infixl 4 <??> 
  in Left "AA" <??> Right (+1)  -- True
  -- in Left "AA" <??> Right (+1)  -- True

exprPair :: (forall a b . (String,a) -> (String,a -> b) -> (String,b)) -> (String,Int)
exprPair op = 
  let (<??>) = op 
      infixl 4 <??> 
  -- in ("AA", 3) <??> ("",(+1))  -- True
  in ("AA", 3) <??> ("B",(+1))  -- place for counterexample

exprEnv :: (forall a b . (String -> a) -> (String -> (a -> b)) -> (String -> b)) -> (String -> Int)
exprEnv op = 
  let (<??>) = op 
      infixl 4 <??> 
  in length <??> (\_ -> (+5))  -- place for counterexample
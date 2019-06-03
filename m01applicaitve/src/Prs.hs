module Prs where  

import Control.Applicative
import Data.Char

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }
instance Functor Prs where
  fmap f (Prs p) = Prs ((fmap . fmap) (\(r, tail) -> (f r, tail)) p)

instance Applicative Prs where
  pure x = Prs (\s -> Just (x, s))
  pf <*> pv = Prs fun where
    fun s = do
      (g, s') <- runPrs pf s
      (a, s'') <- runPrs pv s'
      return (g a, s'')

instance Alternative Prs where
  empty = Prs f where
      f _ = Nothing 
  p <|> q = Prs f where
    f s = runPrs p s <|> runPrs q s

anyChr :: Prs Char
anyChr = Prs (\s -> 
  case s of [] -> Nothing
            (x:xs) -> Just (x, xs))

many1 :: Prs a -> Prs [a]
many1 p = (:) <$> p <*> many p 

char :: Char -> Prs Char
char c = Prs (\s -> 
  case s of [] -> Nothing
            (x:xs) -> if x == c then Just (x, xs) else Nothing)

nat :: Prs Int
nat = Prs(\s -> 
  case span isDigit s of ([], tail) -> Nothing
                         (digits, tail) -> Just (read digits, tail)
  )

mult :: Prs Int
mult = (*) <$> nat <* char '*' <*> nat            
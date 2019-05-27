module PrsE where

newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

instance Functor PrsE where
  fmap f (PrsE p) = PrsE ((fmap . fmap) (\(r, tail) -> (f r, tail)) p)

instance Applicative PrsE where
  pure x = PrsE (\s -> Right (x, s))
  pf <*> pv = PrsE fun where
    fun s = do
      (g, s') <- runPrsE pf s
      (a, s'') <- runPrsE pv s'
      return (g a, s'')

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE pr = PrsE f where
  f "" = Left "unexpected end of input"
  f (c:cs) | pr c = Right (c, cs)
           | otherwise = Left ("unexpected " ++ [c])

charE :: Char -> PrsE Char
charE c = satisfyE (== c)
module MonadParser where

newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

instance Functor PrsE where
  fmap f (PrsE prs) = PrsE ((fmap . fmap) (\(r, tail) -> (f r, tail)) prs)

instance Applicative PrsE where
  pure x = PrsE (\s -> Right (x, s))
  pf <*> pv = PrsE fun where
    fun s = do
      (g, s') <- runPrsE pf s
      (a, s'') <- runPrsE pv s'
      return (g a, s'')

instance Monad PrsE where
  (>>=) (PrsE p) f = PrsE (\s -> 
    case (p s) of
      Left err        -> Left err
      Right (a, tail) -> (runPrsE (f a)) tail) 
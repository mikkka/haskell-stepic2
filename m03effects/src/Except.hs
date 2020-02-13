module Except where
import Control.Monad (liftM, ap, MonadPlus(mzero, mplus), guard, msum)
import Control.Applicative (Alternative(empty, (<|>)))
newtype Except e a = Except {runExcept :: Either e a} deriving Show

except :: Either e a -> Except e a
except = Except

withExcept :: (e -> e') -> Except  e a -> Except e' a
withExcept f (Except (Left e)) = except $ Left $ f e
withExcept _ (Except (Right a)) = except $ Right a

throwE :: e -> Except e a
throwE = except . Left

catchE :: Except e a -> (e -> Except e' a) -> Except e' a
m `catchE` h = 
  case runExcept m of 
    Left e  -> h e
    Right r -> except $ Right r

instance Functor (Except e) where
  fmap = liftM

instance Applicative (Except e) where 
  pure  = return 
  (<*>) = ap

instance Monad (Except e) where 
  return        = Except . Right
  exce >>= f    = 
    case runExcept exce of
      Left err -> Except $ Left err
      Right x  -> f x

instance Monoid e => Alternative (Except e) where
  empty                 = Except $ Left mempty 
  Except (Left _) <|> n = n
  m               <|> _ = m

instance Monoid e => MonadPlus (Except e) where

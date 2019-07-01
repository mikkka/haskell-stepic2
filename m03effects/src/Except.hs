module Except where

newtype Except e a = Except {runExcept :: Either e a} deriving Show

except :: Either e a -> Except e a
except = Except

withExcept :: (e -> e') -> Except  e a -> Except e' a
withExcept f (Except (Left e)) = Except $ Left $ f e
withExcept _ (Except (Right a)) = Except $ Right $ a
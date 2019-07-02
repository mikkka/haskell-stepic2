module ListIdx where

import Control.Monad.Trans.Except
import Data.Foldable

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex 
  deriving (Eq, Show)

(!!!) :: [a] -> Int -> Except ListIndexError a
(!!!) list index = 
  if index < 0 then except $ Left ErrNegativeIndex
  else scan list index
    where
      scan xs idx = case xs of 
        []       -> except $ Left $ (ErrIndexTooLarge index)
        (x:tail) -> if idx == 0 then except $ Right x else scan tail (idx - 1)  


newtype SimpleError = Simple { getSimple :: String } 
  deriving (Eq, Show)
      
instance Semigroup SimpleError where 
  (<>) s1 s2 = s1 `mappend` s2

instance Monoid SimpleError where
  mempty = Simple ""
  mappend (Simple s1) (Simple s2) = Simple $ s1 ++ s2

lie2se :: ListIndexError -> SimpleError
lie2se ErrNegativeIndex     = Simple "[negative index]"
lie2se (ErrIndexTooLarge i) = Simple $ "[index (" ++ show i ++ ") is too large]"
  
toSimple = runExcept . withExcept lie2se
toSimpleFromList = runExcept . msum . map (withExcept lie2se)


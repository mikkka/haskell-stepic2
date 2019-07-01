module ListIdx where

import Control.Monad.Trans.Except

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
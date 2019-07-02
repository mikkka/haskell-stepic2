module ReadExcept where

import Control.Monad.Trans.Except

data ReadError = EmptyInput | NoParse String
  deriving Show

tryRead :: Read a => String -> Except ReadError a
tryRead ""  = throwE EmptyInput
tryRead str = 
    case (reads str) of
      ((x, "") : _)  -> return x
      []             -> throwE $ NoParse str 
  
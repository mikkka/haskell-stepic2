module TryRead where

import ExceptT

data ReadError = EmptyInput | NoParse String
  deriving Show

tryRead :: (Read a, Monad m) => String -> ExceptT ReadError m a
tryRead ""  = throwE EmptyInput
tryRead str =
    case reads str of
      ((x, "") : _)  -> return x
      _              -> throwE $ NoParse str
